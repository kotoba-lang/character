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
  (let [skel (body/generate-humanoid-skeleton)]
    (is (= 13 (count (:bones skel))))
    (is (nil? (:parent (first (:bones skel)))))
    (is (= "hips" (:name (first (:bones skel)))))))

;; ── skinning (/loop maturity pass, ADR-2607031200) ───────────────────────

(deftest test-bone-world-positions
  (let [bones (:bones (body/generate-humanoid-skeleton))
        wp (body/bone-world-positions bones)]
    (is (= 13 (count wp)))
    ;; hips is root: local-position IS world-position.
    (is (= (:local-position (first bones)) (first wp)))
    ;; spine = hips + spine's own local-position (both y-negative-going-up
    ;; chain sums correctly — spot-check against hand-accumulated value).
    (is (< (Math/abs (- (second (nth wp 1)) -0.12)) 1e-9))
    ;; leftUpperArm (idx 10) is 2 levels below neck: neck -> leftShoulder ->
    ;; leftUpperArm, so its world x should be more negative (further left)
    ;; than leftShoulder's (idx 9).
    (is (< (first (nth wp 10)) (first (nth wp 9))))))

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
