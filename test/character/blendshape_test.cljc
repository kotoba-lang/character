(ns character.blendshape-test
  "Supplementary sanity tests for `character.blendshape` — the Rust
  `blendshape.rs` had no `#[cfg(test)]` block to port 1:1."
  (:require [clojure.test :refer [deftest is]]
            [character.blendshape :as blendshape]
            [character.base-mesh :as base-mesh]
            [character.params :as params]))

(deftest test-arkit-names-count
  (is (= 52 (count blendshape/arkit-names))))

(deftest test-generate-arkit-targets
  (let [targets (blendshape/generate-arkit-targets 10)]
    (is (= 52 (count targets)))
    (is (= "eyeBlinkLeft" (:name (first targets))))
    (is (= "tongueOut" (:name (last targets))))
    (doseq [t targets]
      (is (= 10 (count (:deltas t)))))))

(deftest test-apply-face-shape-preserves-count
  (let [def1 (params/default-character-def)
        {:keys [vertices]} (base-mesh/generate-head 6 6)
        shaped (blendshape/apply-face-shape vertices (:face def1))]
    (is (= (count vertices) (count shaped)))
    (doseq [[x y z] shaped]
      (is (and (Double/isFinite x) (Double/isFinite y) (Double/isFinite z))))))

(deftest test-apply-eye-nose-mouth-shape
  (let [def1 (params/default-character-def)
        {:keys [vertices]} (base-mesh/generate-head 6 6)
        v1 (blendshape/apply-eye-shape vertices (:eyes def1))
        v2 (blendshape/apply-nose-shape v1 (:nose def1))
        v3 (blendshape/apply-mouth-shape v2 (:mouth def1))]
    (is (= (count vertices) (count v3)))))
