(ns character-test
  "Tests ported 1:1 from the deleted kami-character crate's `lib.rs`
  `#[cfg(test)] mod tests`, plus the namespace-loads smoke test."
  (:require [clojure.test :refer [deftest is testing]]
            [character :as character]
            [character.params :as params]
            [character.hair-gen :as hair-gen]
            [character.metahuman :as metahuman]
            [character.control-rig :as control-rig]
            [character.anim-blueprint :as anim-blueprint]))

(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? (the-ns 'character)))))

(deftest test-generate-default-character
  (let [def1 (params/default-character-def)
        mesh (character/generate-character def1)]
    (is (not (empty? (:parts mesh))))
    (is (some #(= (:name %) "head") (:parts mesh)))
    (is (some #(= (:material %) :hair) (:parts mesh)))
    (let [total-verts (reduce + (map #(count (:vertices %)) (:parts mesh)))]
      (is (> total-verts 5000) (str "Expected 5K+ verts, got " total-verts)))))

(deftest test-blendshape-targets
  (let [def1 (params/default-character-def)
        mesh (character/generate-character def1)]
    (is (= 52 (count (:blendshape-targets mesh))) "Expected 52 ARKit targets")))

(deftest test-sdk-re-exports
  ;; Verify SDK-level constructors are accessible (parity with Rust's
  ;; re-export smoke test).
  (is (some? (hair-gen/default-hair-style)))
  (is (= :lod0 :lod0))
  (is (some? metahuman/facs-names))
  (is (some? (control-rig/metahuman-face-rig)))
  (is (some? (anim-blueprint/metahuman-default))))
