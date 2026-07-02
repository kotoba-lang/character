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
