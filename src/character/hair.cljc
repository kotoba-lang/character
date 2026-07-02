(ns character.hair
  "Hair mesh generation — preset-based strand quad strips. Restored
  from the legacy kami-engine/kami-character Rust crate's `hair.rs`
  (deleted in kotoba-lang/kami-engine PR #82) as part of the clj-wgsl
  migration (ADR-2607010930, com-junkawasaki/root)."
  (:require [character.math :as m]))

(def hash-f32 m/hash-f32)

(defn- preset-config
  "Returns `[n-strands n-segments base-length]` for a hair preset."
  [preset]
  (case preset
    :bald [0 0 0.0]
    :buzz [200 3 0.015]
    :pixie [150 6 0.04]
    (:short-straight :short-wavy :short-curly) [150 8 0.07]
    :bob [180 10 0.10]
    (:medium-straight :medium-wavy :medium-layered) [200 10 0.15]
    (:long-straight :long-wavy :long-curly) [250 12 0.25]
    (:ponytail-high :ponytail-low) [150 12 0.20]
    (:bun-top :bun-low) [120 8 0.06]
    (:undercut :mohawk) [100 8 0.07]
    :afro-short [300 5 0.04]
    :afro-large [400 6 0.07]
    (:braids-twin :braids-single) [120 12 0.22]))

(defn- strand-origin
  "Compute strand origin `[theta phi]` based on preset and strand index."
  [preset idx _total _part-pos]
  (let [h (hash-f32 idx 42)
        h2 (hash-f32 idx 99)]
    (case preset
      :bald [0.0 0.0]
      :mohawk (let [theta (+ m/pi (* (- h 0.5) 0.3))
                    phi (* h2 m/pi 0.35)]
                [theta phi])
      (let [theta (if (> h 0.3)
                    (* m/pi (+ 0.2 (* 1.6 h2)))
                    (* 2.0 m/pi h2))
            phi (* h m/pi 0.45)]
        [theta phi]))))

(defn generate-hair
  "Generate hair mesh from HairParams map."
  [{:keys [preset length-scale volume part-position]}]
  (let [[n-strands n-segments base-length] (preset-config preset)
        length (* base-length length-scale)]
    (if (zero? n-strands)
      {:name "hair" :vertices [] :indices [] :material :hair}
      (let [strand-results
            (for [s (range n-strands)]
              (let [[start-theta start-phi] (strand-origin preset s n-strands part-position)
                    sx (* (+ 0.095 (* volume 0.01)) (m/sin start-phi) (m/cos start-theta))
                    sy (* 0.125 (m/cos start-phi))
                    sz (* (+ 0.085 (* volume 0.005)) (m/sin start-phi) (m/sin start-theta))
                    w0 (+ 0.001 (* volume 0.002))
                    curl (- (* (hash-f32 s 0) 0.4) 0.2)
                    segs
                    (for [seg (range n-segments)]
                      (let [t (/ (double seg) (dec n-segments))
                            w (* w0 (- 1.0 (* t 0.8)))
                            x (+ sx (* (hash-f32 s (+ seg 100)) 0.004 t) (* t 0.008 (m/sin (+ curl (* t 1.5)))))
                            y (- sy (* t length))
                            z (+ sz (* (hash-f32 s (+ seg 200)) 0.004 t) (* t 0.005 (m/sin start-theta)))
                            right (m/vec3-scale [(m/cos (+ start-theta (/ m/pi 2.0))) 0.0 (m/sin (+ start-theta (/ m/pi 2.0)))] w)
                            pos-l [(- x (nth right 0)) y (- z (nth right 2))]
                            pos-r [(+ x (nth right 0)) y (+ z (nth right 2))]
                            center-dir (m/vec3-normalize-or-zero [x 0.0 z])]
                        {:pos-l pos-l :pos-r pos-r :normal center-dir :t t}))]
                segs))
            vertices
            (vec (mapcat (fn [segs]
                           (mapcat (fn [{:keys [pos-l pos-r normal t]}]
                                     [{:position pos-l :normal normal :uv [0.0 t]}
                                      {:position pos-r :normal normal :uv [1.0 t]}])
                                   segs))
                         strand-results))
            indices
            (vec (mapcat (fn [strand-idx]
                           (mapcat (fn [seg]
                                     (if (pos? seg)
                                       (let [i (+ (* strand-idx n-segments 2) (* (dec seg) 2))]
                                         [i (+ i 2) (+ i 1) (+ i 1) (+ i 2) (+ i 3)])
                                       []))
                                   (range n-segments)))
                         (range n-strands)))]
        {:name "hair" :vertices vertices :indices indices :material :hair}))))
