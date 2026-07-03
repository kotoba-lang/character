(ns character.body
  "Body + clothing mesh generation + humanoid skeleton. Restored from
  the legacy kami-engine/kami-character Rust crate's `body.rs`
  (deleted in kotoba-lang/kami-engine PR #82) as part of the clj-wgsl
  migration (ADR-2607010930, com-junkawasaki/root)."
  (:require [character.math :as m]))

(defn- ring-mesh
  "Shared ring/cylinder mesh builder used by both body and clothing:
  `radius-fn` takes `t` (ring progress 0..1) and returns `[rx rz]`."
  [n-rings n-seg y-fn radius-fn]
  (let [vertices
        (vec
         (for [i (range (inc n-rings))
               :let [t (/ (double i) n-rings)
                     y (y-fn t)
                     [rx rz] (radius-fn t)]
               j (range (inc n-seg))]
           (let [theta (* 2.0 m/pi (/ (double j) n-seg))
                 x (* rx (m/cos theta))
                 z (* rz (m/sin theta))
                 n (m/vec3-normalize [(m/cos theta) 0.0 (m/sin theta)])]
             {:position [x y z] :normal n :uv [(/ (double j) n-seg) t]})))
        indices
        (vec
         (mapcat
          (fn [i]
            (mapcat
             (fn [j]
               (let [a (+ (* i (inc n-seg)) j)
                     b (+ a n-seg 1)]
                 [a b (inc a) (inc a) b (inc b)]))
             (range n-seg)))
          (range n-rings)))]
    [vertices indices]))

(defn generate-body
  "Generate neck + upper body mesh (BodyParams map)."
  [{:keys [neck-thickness shoulder-width build height] :as params}]
  (let [n-rings 20 n-seg 28
        neck-thick (+ 0.035 (* neck-thickness 0.02))
        shoulder-w (+ 0.1 (* shoulder-width 0.08))
        y-fn (fn [t] (- -0.12 (* t 0.28 height)))
        radius-fn (fn [t]
                    (cond
                      (< t 0.2) [(+ neck-thick (* t 0.06)) (+ (* neck-thick 0.85) (* t 0.05))]
                      (< t 0.5) (let [s (- t 0.2)]
                                  [(+ neck-thick 0.012 (* s (/ (- shoulder-w neck-thick) 0.3)))
                                   (+ (* neck-thick 0.85) 0.01 (* s 0.1))])
                      :else (let [s (- t 0.5)]
                              [(+ shoulder-w (* s 0.02) (* build 0.02))
                               (+ 0.08 (* build 0.03) (* s 0.01))])))
        [vertices indices] (ring-mesh n-rings n-seg y-fn radius-fn)]
    {:name "body" :vertices vertices :indices indices :material :skin}))

;; ── skinning (ADR-2607031200, /loop maturity pass) ───────────────────────
;; `generate-body`'s mesh had no vertex skin weights (no JOINTS_0/WEIGHTS_0),
;; so a viewer could not pose-deform it even though the exported VRM's
;; humanoid bone *nodes* were correctly mapped. `bone-world-positions` +
;; `skin-weights` close that gap via standard "bind by proximity"
;; auto-skinning: each vertex binds to its 4 nearest bones (rest-pose
;; distance), weighted by inverse-square-distance and normalized to sum 1.0.
;; This is a documented approximation (no hand-painted weights, no captured
;; rig data) — the same honesty convention as `character-creator.expression-
;; bridge`'s ARKit<->VRM table. Smooth falloff at joint boundaries (shoulder,
;; neck) falls out naturally from the inverse-distance weighting; no special
;; boundary-case code is needed.

(defn bone-world-positions
  "World-space rest position for every bone in `bones` (a
  `generate-humanoid-skeleton`-shaped seq), computed by summing
  `:local-position` up each bone's parent chain. Correct because every bone
  here uses `identity-rot` (translation-only composition) — a future skeleton
  with non-identity rest rotations would need a real parent-rotated TRS
  compose instead of plain vector addition. Returns a vector index-aligned
  with `bones`."
  [bones]
  (let [bones (vec bones)]
    (mapv (fn [i]
            (loop [i i acc [0.0 0.0 0.0]]
              (let [{:keys [parent local-position]} (bones i)
                    acc (m/vec3+ acc local-position)]
                (if parent (recur parent acc) acc))))
          (range (count bones)))))

(defn skin-weights
  "Per-vertex `{:joint-indices [i0 i1 i2 i3] :joint-weights [w0 w1 w2 w3]}`
  (glTF JOINTS_0/WEIGHTS_0 convention) for `vertices` against `bone-world-pos`
  (from `bone-world-positions`) — inverse-square-distance auto-skinning to
  the 4 nearest bones, normalized to sum ~1.0. Unused influence slots (fewer
  than 4 bones available) are padded `0`/`0.0`."
  [vertices bone-world-pos]
  (let [n (count bone-world-pos)
        k (min 4 n)]
    (mapv
     (fn [{:keys [position]}]
       (let [ranked (->> bone-world-pos
                          (map-indexed (fn [i bp] [i (max 1e-4 (m/vec3-length (m/vec3- position bp)))]))
                          (sort-by second)
                          (take k))
             inv-w (mapv (fn [[_ d]] (/ 1.0 (* d d))) ranked)
             total (reduce + inv-w)
             norm-w (mapv #(/ % total) inv-w)
             idxs (into (mapv first ranked) (repeat (- 4 k) 0))
             ws (into norm-w (repeat (- 4 k) 0.0))]
         {:joint-indices idxs :joint-weights ws}))
     vertices)))

(defn skin-body
  "`generate-body`'s `MeshPart` map, with each vertex's `:joint-indices`/
  `:joint-weights` attached (mutates via `skin-weights` against `bones`'
  rest-pose world positions)."
  [body-part bones]
  (let [bwp (bone-world-positions bones)
        weights (skin-weights (:vertices body-part) bwp)]
    (update body-part :vertices
            (fn [vs] (mapv merge vs weights)))))

(defn generate-clothing
  "Generate clothing mesh (slightly offset from body). `params` is a
  ClothingParams map, `body` a BodyParams map."
  [{:keys [preset fit]} {:keys [shoulder-width height build] :as _body}]
  (let [n-rings 16 n-seg 24
        offset (+ 0.004 (* fit 0.003))
        shoulder-w (+ 0.1 (* shoulder-width 0.08) offset)
        [start-t coverage]
        (case preset
          (:tank-top :nude-shoulders) [0.35 0.65]
          (:t-shirt :blouse) [0.25 0.75]
          (:hoodie :jacket) [0.15 0.85]
          [0.25 0.75])
        y-fn (fn [t] (let [tt (+ start-t (* coverage t))] (- -0.12 (* tt 0.28 height))))
        radius-fn (fn [t]
                    (let [tt (+ start-t (* coverage t))
                          rx (+ (if (< tt 0.5) (* shoulder-w tt 2.0) (+ shoulder-w (* (- tt 0.5) 0.02))) offset)
                          rz (+ 0.08 (* build 0.03) offset)]
                      [rx rz]))
        [vertices indices] (ring-mesh n-rings n-seg y-fn radius-fn)]
    {:name "clothing" :vertices vertices :indices indices :material :clothing}))

(defn bone
  [name parent local-position local-rotation]
  {:name name :parent parent :local-position local-position
   :local-rotation local-rotation :local-scale m/vec3-one :inverse-bind m/mat4-identity})

(def identity-rot [0.0 0.0 0.0 1.0])

(defn generate-humanoid-skeleton
  "Generate VRM 1.0-compatible humanoid skeleton (13 core bones)."
  []
  {:bones
   [(bone "hips" nil [0.0 -0.2 0.0] identity-rot)
    (bone "spine" 0 [0.0 0.08 0.0] identity-rot)
    (bone "chest" 1 [0.0 0.08 0.0] identity-rot)
    (bone "upperChest" 2 [0.0 0.06 0.0] identity-rot)
    (bone "neck" 3 [0.0 0.06 0.0] identity-rot)
    (bone "head" 4 [0.0 0.06 0.0] identity-rot)
    (bone "leftEye" 5 [-0.03 0.04 0.06] identity-rot)
    (bone "rightEye" 5 [0.03 0.04 0.06] identity-rot)
    (bone "jaw" 5 [0.0 -0.02 0.04] identity-rot)
    (bone "leftShoulder" 3 [-0.04 0.04 0.0] identity-rot)
    (bone "leftUpperArm" 9 [-0.06 0.0 0.0] identity-rot)
    (bone "rightShoulder" 3 [0.04 0.04 0.0] identity-rot)
    (bone "rightUpperArm" 11 [0.06 0.0 0.0] identity-rot)]})
