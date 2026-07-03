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

;; ── mesh-part transform helpers (full-body limbs, /loop maturity pass) ───
;; `ring-mesh` only ever builds a mesh whose long axis is local +/-Y (a
;; straight vertical cylinder/revolve). Legs are already oriented that way
;; (hang straight down from the hip), so they only need a translation. Arms
;; extend sideways from the shoulder, so an arm is built as the SAME vertical
;; cylinder, then rotated 90 degrees about Z to redirect its long axis to
;; +/-X, then translated to the shoulder attachment point — reusing `ring-
;; mesh` unchanged rather than writing a second, differently-oriented
;; revolve builder.

(defn- rotate-vec3
  "Rotate `v` by quaternion `q` (via `quat-to-mat3-cols`'s column basis)."
  [q v]
  (let [[c0 c1 c2] (m/quat-to-mat3-cols q)]
    (m/vec3+ (m/vec3+ (m/vec3-scale c0 (nth v 0)) (m/vec3-scale c1 (nth v 1)))
             (m/vec3-scale c2 (nth v 2)))))

(defn- offset-mesh-part
  "Translate every vertex `:position` (not `:normal`) by `offset`."
  [{:keys [vertices] :as mesh-part} offset]
  (assoc mesh-part :vertices
         (mapv (fn [v] (update v :position m/vec3+ offset)) vertices)))

(defn- rotate-translate-mesh-part
  "Rotate every vertex's `:position`/`:normal` by `q`, then translate
  `:position` by `offset`."
  [{:keys [vertices] :as mesh-part} q offset]
  (assoc mesh-part :vertices
         (mapv (fn [{:keys [position normal] :as v}]
                 (assoc v :position (m/vec3+ (rotate-vec3 q position) offset)
                        :normal (rotate-vec3 q normal)))
               vertices)))

(defn- merge-mesh-parts
  "Concatenate several `{:vertices :indices}` maps into one, offsetting each
  part's index array by the running vertex count so the combined index
  buffer stays valid."
  [name material parts]
  (let [[vertices indices]
        (reduce (fn [[vs is] {pv :vertices pi :indices}]
                  (let [base (count vs)]
                    [(into vs pv) (into is (map #(+ % base) pi))]))
                [[] []]
                parts)]
    {:name name :vertices vertices :indices indices :material material}))

;; ── humanoid skeleton ─────────────────────────────────────────────────

(defn bone
  [name parent local-position local-rotation]
  {:name name :parent parent :local-position local-position
   :local-rotation local-rotation :local-scale m/vec3-one :inverse-bind m/mat4-identity})

(def identity-rot [0.0 0.0 0.0 1.0])

(defn generate-humanoid-skeleton
  "Generate a VRM 1.0-compatible humanoid skeleton: the original 13 core
  bones (hips through eyes/jaw/shoulders/upper-arms, torso proportions
  unscaled by `height` — unchanged from before this fn grew a `height` arg)
  plus, new for the /loop maturity pass (ADR-2607031200), 10 more so the
  figure is a real standing body instead of a bust: `leftUpperLeg`/
  `leftLowerLeg`/`leftFoot` + their `right*` mirrors (children of `hips`),
  and `leftLowerArm`/`leftHand` + their `right*` mirrors (children of
  `leftUpperArm`/`rightUpperArm`, bones 10/12). Limb SEGMENT LENGTHS scale
  with `height` (arg, default `1.0` for the pre-existing 0-arity callers);
  attachment points (hips/shoulders) keep their original fixed offsets,
  matching how the torso mesh already only height-scales its vertical
  extent, not its attachment. Bone-name strings are the real VRM 1.0
  humanoid names (`vrm.vrm-types/str->human-bone-name`'s table), not
  invented — so `character-creator.pipeline`'s VRMC_vrm humanoid mapping
  picks them up unchanged."
  ([] (generate-humanoid-skeleton 1.0))
  ([height]
   (let [thigh (* 0.11 height) shin (* 0.10 height) foot 0.06
         upper-arm-ext (* 0.10 height) forearm (* 0.09 height)]
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
       (bone "rightUpperArm" 11 [0.06 0.0 0.0] identity-rot)
       ;; idx 13-18: legs (children of hips = 0)
       (bone "leftUpperLeg" 0 [-0.045 -0.02 0.0] identity-rot)
       (bone "leftLowerLeg" 13 [0.0 (- thigh) 0.0] identity-rot)
       (bone "leftFoot" 14 [0.0 (- shin) foot] identity-rot)
       (bone "rightUpperLeg" 0 [0.045 -0.02 0.0] identity-rot)
       (bone "rightLowerLeg" 16 [0.0 (- thigh) 0.0] identity-rot)
       (bone "rightFoot" 17 [0.0 (- shin) foot] identity-rot)
       ;; idx 19-22: forearms/hands (children of leftUpperArm=10 / rightUpperArm=12)
       (bone "leftLowerArm" 10 [(- upper-arm-ext) 0.0 0.0] identity-rot)
       (bone "leftHand" 19 [(- forearm) 0.0 0.0] identity-rot)
       (bone "rightLowerArm" 12 [upper-arm-ext 0.0 0.0] identity-rot)
       (bone "rightHand" 21 [forearm 0.0 0.0] identity-rot)]})))

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

;; ── body mesh: torso + legs + arms (full standing figure) ────────────────

(defn- torso-mesh
  "Torso ring-mesh: `t=0` (top) at neck-bone height (`0.08`, world) down to
  `t=1` (bottom) at hip-bone height (`0.08 - 0.28*height` = `-0.20` when
  `height=1.0` — exactly the hips bone's world y). Before the /loop
  maturity pass this spanned `[-0.12, -0.12-0.28*height]`, i.e. neck down to
  waist-ish, which never reached `chest`/`upperChest` (both above `-0.12`) —
  the real cause of the original 'almost every vertex's dominant bone is
  hips or spine' finding. Shifting the span up by `0.20` (same `0.28`
  height-coefficient, so the total drop is unchanged) makes the torso
  actually cover the chest/upperChest/shoulder bones it visually should."
  [{:keys [neck-thickness shoulder-width build height]}]
  (let [neck-thick (+ 0.035 (* neck-thickness 0.02))
        shoulder-w (+ 0.1 (* shoulder-width 0.08))
        y-fn (fn [t] (- 0.08 (* t 0.28 height)))
        radius-fn (fn [t]
                    (cond
                      (< t 0.2) [(+ neck-thick (* t 0.06)) (+ (* neck-thick 0.85) (* t 0.05))]
                      (< t 0.5) (let [s (- t 0.2)]
                                  [(+ neck-thick 0.012 (* s (/ (- shoulder-w neck-thick) 0.3)))
                                   (+ (* neck-thick 0.85) 0.01 (* s 0.1))])
                      :else (let [s (- t 0.5)]
                              [(+ shoulder-w (* s 0.02) (* build 0.02))
                               (+ 0.08 (* build 0.03) (* s 0.01))])))
        [vertices indices] (ring-mesh 20 28 y-fn radius-fn)]
    {:vertices vertices :indices indices}))

(defn- leg-mesh
  "One leg: a tapering vertical cylinder from hip level (`t=0`) down through
  the (implicit) knee to the ankle (`t=1`), translated out to `hip-x` (the
  matching `leftUpperLeg`/`rightUpperLeg` bone's world x). Length
  `thigh+shin` matches `generate-humanoid-skeleton`'s leg bone offsets so
  the mesh's ankle and the `*Foot` bone land at roughly the same height."
  [build height hip-world hip-x-local]
  (let [len (* 0.21 height)
        y-fn (fn [t] (- (* t len)))
        radius-fn (fn [t] (let [r (- 0.045 (* t 0.016))] [(+ r (* build 0.012)) (+ r (* build 0.012))]))
        [vertices indices] (ring-mesh 10 16 y-fn radius-fn)]
    (offset-mesh-part {:vertices vertices :indices indices}
                       (m/vec3+ hip-world [hip-x-local 0.0 0.0]))))

(defn- arm-mesh
  "One arm: the same vertical-cylinder technique as `leg-mesh`, but rotated
  90 degrees about Z (`side` `-1`=left maps `+Y` to world `-X`, `+1`=right
  maps to `+X` — verified against this file's own left=`-X`/right=`+X`
  convention, e.g. `leftShoulder`'s `-0.04`) before being translated to the
  `leftUpperArm`/`rightUpperArm` bone's world position (the shoulder-to-
  elbow attachment point)."
  [side build height upper-arm-world]
  (let [len (* 0.19 height)
        y-fn (fn [t] (- (* t len)))
        radius-fn (fn [t] (let [r (- 0.030 (* t 0.011))] [(+ r (* build 0.007)) (+ r (* build 0.007))]))
        [vertices indices] (ring-mesh 8 14 y-fn radius-fn)
        q (m/quat-from-axis-angle [0.0 0.0 1.0] (* side (/ m/pi 2.0)))]
    (rotate-translate-mesh-part {:vertices vertices :indices indices} q upper-arm-world)))

(defn generate-body
  "Generate a full standing-body mesh (torso + 2 legs + 2 arms, merged into
  one `MeshPart` named `\"body\"`) — extended (/loop maturity pass,
  ADR-2607031200) from the original neck+upper-body-only bust. `params` is
  a `BodyParams` map (`:height`/`:shoulder-width`/`:build`/`:neck-
  thickness`, unchanged shape). `bones` (2-arity; defaults to
  `(:bones (generate-humanoid-skeleton (:height params)))` in the 1-arity
  form, matching the original call convention) supplies the hip/shoulder
  attachment points legs/arms are placed relative to — geometry and
  skeleton share one source of truth instead of duplicating hardcoded
  offsets. Reuses `ring-mesh` for every limb (no second revolve-mesh
  implementation); arms use `rotate-translate-mesh-part` to redirect the
  same vertical cylinder sideways rather than a differently-shaped builder."
  ([params] (generate-body params (:bones (generate-humanoid-skeleton (:height params)))))
  ([{:keys [build height] :as params} bones]
   (let [bwp (bone-world-positions bones)
         idx-by-name (into {} (map-indexed (fn [i b] [(:name b) i]) bones))
         at (fn [n] (nth bwp (idx-by-name n)))
         torso (torso-mesh params)
         l-leg (leg-mesh build height (at "hips") -0.045)
         r-leg (leg-mesh build height (at "hips") 0.045)
         l-arm (arm-mesh -1.0 build height (at "leftUpperArm"))
         r-arm (arm-mesh 1.0 build height (at "rightUpperArm"))]
     (merge-mesh-parts "body" :skin [torso l-leg r-leg l-arm r-arm]))))

(defn generate-clothing
  "Generate clothing mesh (slightly offset from body, torso region only —
  legs/arms added to `generate-body` this session are not yet covered by
  any clothing preset; out of scope for the /loop maturity pass that added
  them). `params` is a ClothingParams map, `body` a BodyParams map."
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
        y-fn (fn [t] (let [tt (+ start-t (* coverage t))] (- 0.08 (* tt 0.28 height))))
        radius-fn (fn [t]
                    (let [tt (+ start-t (* coverage t))
                          rx (+ (if (< tt 0.5) (* shoulder-w tt 2.0) (+ shoulder-w (* (- tt 0.5) 0.02))) offset)
                          rz (+ 0.08 (* build 0.03) offset)]
                      [rx rz]))
        [vertices indices] (ring-mesh n-rings n-seg y-fn radius-fn)]
    {:name "clothing" :vertices vertices :indices indices :material :clothing}))
