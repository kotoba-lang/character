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
