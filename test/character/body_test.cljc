(ns character.body-test
  "Supplementary sanity tests for `character.body` — the Rust
  `body.rs` had no `#[cfg(test)]` block to port 1:1."
  (:require [clojure.test :refer [deftest is]]
            [character.body :as body]
            [character.params :as params]))

(deftest test-generate-body
  (let [def1 (params/default-character-def)
        part (body/generate-body (:body def1))]
    (is (= "body" (:name part)))
    (is (= :skin (:material part)))
    (is (not (empty? (:vertices part))))
    (is (not (empty? (:indices part))))))

(deftest test-generate-clothing-presets
  (let [def1 (params/default-character-def)]
    (doseq [preset params/clothing-presets]
      (let [clothing (assoc (:clothing def1) :preset preset)
            part (body/generate-clothing clothing (:body def1))]
        (is (= "clothing" (:name part)))
        (is (not (empty? (:vertices part))))))))

(deftest test-generate-humanoid-skeleton
  ;; 13 original core bones + 10 added for the /loop maturity pass full-body
  ;; extension (6 leg bones + 4 forearm/hand bones) = 23.
  (let [skel (body/generate-humanoid-skeleton)]
    (is (= 23 (count (:bones skel))))
    (is (nil? (:parent (first (:bones skel)))))
    (is (= "hips" (:name (first (:bones skel)))))))

;; ── skinning (/loop maturity pass, ADR-2607031200) ───────────────────────

(deftest test-bone-world-positions
  (let [bones (:bones (body/generate-humanoid-skeleton))
        wp (body/bone-world-positions bones)]
    (is (= 23 (count wp)))
    ;; hips is root: local-position IS world-position.
    (is (= (:local-position (first bones)) (first wp)))
    ;; spine = hips + spine's own local-position (both y-negative-going-up
    ;; chain sums correctly — spot-check against hand-accumulated value).
    (is (< (Math/abs (- (second (nth wp 1)) -0.12)) 1e-9))
    ;; leftUpperArm (idx 10) is 2 levels below neck: neck -> leftShoulder ->
    ;; leftUpperArm, so its world x should be more negative (further left)
    ;; than leftShoulder's (idx 9).
    (is (< (first (nth wp 10)) (first (nth wp 9))))))

;; ── full-body extension (/loop maturity pass, ADR-2607031200) ────────────

(deftest test-leg-arm-bones-sane-rest-positions
  (let [bones (:bones (body/generate-humanoid-skeleton 1.0))
        wp (body/bone-world-positions bones)
        idx-by-name (into {} (map-indexed (fn [i b] [(:name b) i]) bones))
        y-of (fn [n] (second (nth wp (idx-by-name n))))
        x-of (fn [n] (first (nth wp (idx-by-name n))))]
    ;; legs: hip > knee > foot, in world y (each successively lower).
    (is (> (y-of "hips") (y-of "leftUpperLeg")))
    (is (> (y-of "leftUpperLeg") (y-of "leftLowerLeg")))
    (is (> (y-of "leftLowerLeg") (y-of "leftFoot")))
    (is (> (y-of "hips") (y-of "rightFoot")))
    ;; left/right legs mirror in x (left negative, right positive, matching
    ;; this file's existing leftShoulder=-x/rightShoulder=+x convention).
    (is (< (x-of "leftUpperLeg") 0.0))
    (is (> (x-of "rightUpperLeg") 0.0))
    ;; arms: shoulder -> elbow -> hand extends further from the midline
    ;; (larger |x|) at each step, same direction as the upper arm.
    (is (< (x-of "leftLowerArm") (x-of "leftUpperArm")))
    (is (< (x-of "leftHand") (x-of "leftLowerArm")))
    (is (> (x-of "rightLowerArm") (x-of "rightUpperArm")))
    (is (> (x-of "rightHand") (x-of "rightLowerArm")))
    ;; height scales limb length: a taller character's feet/hands sit
    ;; further from their attachment point than a shorter one's.
    (let [wp-tall (body/bone-world-positions (:bones (body/generate-humanoid-skeleton 1.5)))
          wp-short (body/bone-world-positions (:bones (body/generate-humanoid-skeleton 0.6)))]
      (is (< (second (nth wp-tall (idx-by-name "leftFoot")))
             (second (nth wp-short (idx-by-name "leftFoot")))))
      (is (< (first (nth wp-tall (idx-by-name "leftHand")))
             (first (nth wp-short (idx-by-name "leftHand"))))))))

(deftest test-full-body-mesh-not-degenerate
  (let [def1 (params/default-character-def)
        skel (body/generate-humanoid-skeleton (:height (:body def1)))
        part (body/generate-body (:body def1) (:bones skel))
        ys (map #(second (:position %)) (:vertices part))
        finite? (fn [x] #?(:clj (Double/isFinite x) :cljs (js/isFinite x)))]
    ;; a real standing figure, not just the old bust: several hundred
    ;; vertices (torso + 2 legs + 2 arms merged), none NaN/Inf, and a y-range
    ;; that spans from near the neck (torso top, ~0.08) down past where the
    ;; old bust-only mesh ever reached (~-0.12) into the legs.
    (is (> (count (:vertices part)) 1000))
    (is (every? #(and (finite? (first (:position %)))
                       (finite? (second (:position %)))
                       (finite? (nth (:position %) 2)))
                (:vertices part)))
    (is (> (apply max ys) 0.0))
    (is (< (apply min ys) -0.30))))

(deftest test-leg-vertices-dominant-bone-is-leg-not-hips
  ;; The original /loop finding (prior session, bust-only mesh): 522/609
  ;; vertices' dominant bone was "hips", 87/609 "spine" — chest/upperChest
  ;; and every limb bone were unreachable because the mesh never got close
  ;; to them. This is the concrete proof that finding is resolved: a vertex
  ;; built right at a leg midpoint now dominantly binds to a LEG bone.
  (let [def1 (params/default-character-def)
        skel (body/generate-humanoid-skeleton (:height (:body def1)))
        bones (:bones skel)
        wp (body/bone-world-positions bones)
        idx-by-name (into {} (map-indexed (fn [i b] [(:name b) i]) bones))
        leg-lower-pos (nth wp (idx-by-name "leftLowerLeg"))
        fake-vertex [{:position leg-lower-pos :normal [0 0 1] :uv [0 0]}]
        [{:keys [joint-indices joint-weights]}] (body/skin-weights fake-vertex wp)
        dominant (->> (map vector joint-indices joint-weights)
                      (apply max-key second)
                      first)]
    (is (= (idx-by-name "leftLowerLeg") dominant))
    ;; distribution over the WHOLE real mesh: dominant-bone histogram must
    ;; now include leg/arm bones, not just hips/spine/chest.
    (let [part (body/generate-body (:body def1) bones)
          skinned (body/skin-body part bones)
          dominant-name (fn [{:keys [joint-indices joint-weights]}]
                           (:name (bones (->> (map vector joint-indices joint-weights)
                                               (apply max-key second)
                                               first))))
          hist (frequencies (map dominant-name (:vertices skinned)))
          leg-bone-names #{"leftUpperLeg" "leftLowerLeg" "leftFoot"
                            "rightUpperLeg" "rightLowerLeg" "rightFoot"}
          n-leg (reduce + (map #(get hist % 0) leg-bone-names))]
      (is (pos? n-leg))
      ;; hips no longer dominates the WHOLE mesh the way it did pre-extension
      ;; (was 522/609 = 86%); with legs+arms now present it should be a
      ;; minority of a much larger total vertex count.
      (is (< (/ (get hist "hips" 0) (double (count (:vertices skinned)))) 0.5)))))

(deftest test-skin-body-weights-well-formed
  (let [def1 (params/default-character-def)
        skel (body/generate-humanoid-skeleton)
        skinned (body/skin-body (body/generate-body (:body def1)) (:bones skel))
        n-bones (count (:bones skel))]
    (is (pos? (count (:vertices skinned))))
    (doseq [{:keys [joint-indices joint-weights]} (:vertices skinned)]
      (is (= 4 (count joint-indices)))
      (is (= 4 (count joint-weights)))
      (is (every? #(and (>= % 0) (< % n-bones)) joint-indices))
      ;; weights sum to ~1.0 (inverse-distance normalization).
      (is (< (Math/abs (- (reduce + joint-weights) 1.0)) 1e-6))
      (is (every? #(>= % 0.0) joint-weights)))))

(deftest test-skin-body-dominant-joint-is-nearest
  ;; A vertex placed exactly at a bone's rest position must have that bone
  ;; as its highest-weight influence (the core "bind by proximity" claim).
  (let [skel (body/generate-humanoid-skeleton)
        bones (:bones skel)
        wp (body/bone-world-positions bones)
        chest-idx 2 ;; "chest"
        chest-pos (nth wp chest-idx)
        fake-vertex [{:position chest-pos :normal [0 0 1] :uv [0 0]}]
        [{:keys [joint-indices joint-weights]}] (body/skin-weights fake-vertex wp)
        dominant (->> (map vector joint-indices joint-weights)
                      (apply max-key second)
                      first)]
    (is (= chest-idx dominant))))
