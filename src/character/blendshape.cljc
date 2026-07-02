(ns character.blendshape
  "Blendshape system — shape deformations + 52 ARKit expression
  targets. Restored from the legacy kami-engine/kami-character Rust
  crate's `blendshape.rs` (deleted in kotoba-lang/kami-engine PR #82)
  as part of the clj-wgsl migration (ADR-2607010930,
  com-junkawasaki/root)."
  (:require [character.math :as m]))

(defn apply-face-shape
  "Apply face shape blendshapes to base mesh vertices (FaceShapeParams
  map). Returns a new vertex vector."
  [verts {:keys [jaw-width jaw-length chin-shape cheekbone-width forehead-height face-length]}]
  (mapv
   (fn [[x y z]]
     (let [x-abs (Math/abs (double x))
           x (if (< y 0.0)
               (let [t (min 1.0 (/ (- y) 0.12))
                     scale (+ 1.0 (* (- jaw-width 0.5) 0.3 t))]
                 (* x scale))
               x)
           y (if (< y -0.06)
               (let [t (min 1.0 (/ (- (- y) 0.06) 0.06))]
                 (- y (* (- jaw-length 0.5) 0.02 t)))
               y)
           x (if (< y -0.08)
               (let [t (min 1.0 (/ (- (- y) 0.08) 0.04))
                     narrowing (* (- 1.0 chin-shape) 0.3 t)]
                 (* x (- 1.0 (* narrowing (max 0.0 (- 1.0 (/ x-abs 0.05)))))))
               x)
           cheek-y (max 0.0 (- 1.0 (Math/pow (/ (- y 0.01) 0.03) 2)))
           x (if (> cheek-y 0.0)
               (+ x (* (Math/signum (double x)) (- cheekbone-width 0.5) 0.01 cheek-y))
               x)
           y (if (> y 0.08)
               (let [t (min 1.0 (/ (- y 0.08) 0.04))]
                 (+ y (* (- forehead-height 0.5) 0.015 t)))
               y)
           scale-y (+ 1.0 (* (- face-length 0.5) 0.15))
           y (* y scale-y)]
       [x y z]))
   verts))

(defn apply-eye-shape
  "Apply eye socket depth deformations (EyeParams map)."
  [verts {:keys [depth]}]
  (mapv
   (fn [[x y z]]
     (let [z (reduce
              (fn [z eye-x]
                (let [ed (m/sqrt (+ (Math/pow (- x eye-x) 2) (Math/pow (- y 0.045) 2)))]
                  (if (and (< ed 0.025) (> z 0.0))
                    (let [depth-mod (* (- depth 0.5) 0.005)]
                      (- z (* depth-mod (- 1.0 (Math/pow (/ ed 0.025) 2)))))
                    z)))
              z [-0.032 0.032])]
       [x y z]))
   verts))

(defn apply-nose-shape
  "Apply nose shape deformations (NoseParams map)."
  [verts {:keys [bridge-height width]}]
  (mapv
   (fn [[x y z]]
     (let [front (m/clamp (/ z 0.1) 0.0 1.0)
           nose-region (max 0.0 (- 1.0 (Math/pow (/ (- y 0.01) 0.04) 2)))
           center (max 0.0 (- 1.0 (Math/pow (/ x 0.02) 2)))
           z (if (and (> nose-region 0.0) (> center 0.0))
               (+ z (* (- bridge-height 0.5) 0.01 nose-region center front))
               z)
           tip-y (max 0.0 (- 1.0 (Math/pow (/ (+ y 0.005) 0.015) 2)))
           x (if (> tip-y 0.0)
               (+ x (* (Math/signum (double x)) (- width 0.5) 0.005 tip-y front))
               x)]
       [x y z]))
   verts))

(defn apply-mouth-shape
  "Apply mouth shape deformations (MouthParams map)."
  [verts {:keys [width upper-lip-thickness lower-lip-thickness]}]
  (mapv
   (fn [[x y z]]
     (let [front (m/clamp (/ z 0.08) 0.0 1.0)
           lip-y (max 0.0 (- 1.0 (Math/pow (/ (+ y 0.035) 0.015) 2)))
           lip-x (max 0.0 (- 1.0 (Math/pow (/ x 0.03) 2)))]
       (if (and (> lip-y 0.0) (> lip-x 0.0) (> front 0.0))
         (let [x (+ x (* (Math/signum (double x)) (- width 0.5) 0.005 lip-y front))
               thickness (* (+ upper-lip-thickness lower-lip-thickness) 0.5)
               z (+ z (* (- thickness 0.5) 0.004 lip-y lip-x front))]
           [x y z])
         [x y z])))
   verts))

(def arkit-names
  ["eyeBlinkLeft" "eyeBlinkRight"
   "eyeLookDownLeft" "eyeLookDownRight"
   "eyeLookInLeft" "eyeLookInRight"
   "eyeLookOutLeft" "eyeLookOutRight"
   "eyeLookUpLeft" "eyeLookUpRight"
   "eyeSquintLeft" "eyeSquintRight"
   "eyeWideLeft" "eyeWideRight"
   "jawForward" "jawLeft" "jawRight" "jawOpen"
   "mouthClose" "mouthFunnel" "mouthPucker"
   "mouthLeft" "mouthRight"
   "mouthSmileLeft" "mouthSmileRight"
   "mouthFrownLeft" "mouthFrownRight"
   "mouthDimpleLeft" "mouthDimpleRight"
   "mouthStretchLeft" "mouthStretchRight"
   "mouthRollLower" "mouthRollUpper"
   "mouthShrugLower" "mouthShrugUpper"
   "mouthPressLeft" "mouthPressRight"
   "mouthLowerDownLeft" "mouthLowerDownRight"
   "mouthUpperUpLeft" "mouthUpperUpRight"
   "browDownLeft" "browDownRight"
   "browInnerUp"
   "browOuterUpLeft" "browOuterUpRight"
   "cheekPuff" "cheekSquintLeft" "cheekSquintRight"
   "noseSneerLeft" "noseSneerRight"
   "tongueOut"])

(defn generate-arkit-targets
  "Generate 52 ARKit expression blendshape targets. Each target
  contains per-vertex position deltas (zero placeholder — populated
  from captured data or procedural rules)."
  [n-verts]
  (mapv (fn [name] {:name name :deltas (vec (repeat n-verts m/vec3-zero))}) arkit-names))
