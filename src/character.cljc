(ns character
  "KAMI Character — MetaHuman-compatible character SDK for KAMI Engine.
  Restored from the legacy kami-engine/kami-character Rust crate
  (deleted in kotoba-lang/kami-engine PR #82, 'Remove Rust workspace
  from kami-engine') as part of the clj-wgsl migration
  (ADR-2607010930, com-junkawasaki/root). Native execution (wgpu /
  wasmtime / wasmi) stays substrate; this namespace tree owns the
  CLJC contracts / data interpreters / EDN IR for the character
  domain: parametric face/body/hair generation, MetaHuman DNA
  calibration + FACS, control rig, and animation blueprint.

  ## SDK Overview

  ### Character Pipeline
  ```
  Photo -> Murakumo VL -> CharacterDef + HairStyle + MetaHumanDna
    -> generate-character / metahuman/generate-metahuman / dna/from-bytes
    -> CharacterMesh / MetaHumanMesh / TriangulatedMesh
    -> export/export-glb / SkinnedVertex GPU upload
  ```

  ### MetaHuman DNA Pipeline
  ```
  .dna binary (Epic Games) -> dna/from-bytes -> meshes + joints + blend shapes
    -> dna/triangulate-mesh -> positions/normals/UVs/indices (WebGPU ready)
    -> dna/to-skeleton -> bone hierarchy
  ```

  ### Hair Generation Pipeline
  ```
  HairStyle params -> hair-gen/generate-groom -> GroomAsset (strand curves)
    -> groom/to-hair-cards -> HairCard (quad strips for rasterization)
    -> groom/to-kgr / groom/from-kgr -> .kgr binary (storage)
  ```

  ### Face Animation Pipeline
  ```
  FACS AU weights -> control-rig/evaluate -> bone transforms
    -> anim-blueprint/update -> state machine -> blend tree -> pose
  ```

  Namespaces: `character.params` (parametric definitions),
  `character.math` (glam-compatible Vec3/Quat/Mat4), `character.bin`
  (portable binary IO), `character.base-mesh`, `character.blendshape`,
  `character.hair`, `character.body`, `character.material`,
  `character.export` (GLB), `character.metahuman` (DNA/FACS/LOD),
  `character.dna` (.dna parser), `character.skeletal-mesh` (.ksm),
  `character.groom` (.kgr), `character.hair-gen`, `character.control-rig`,
  `character.anim-blueprint`."
  (:require [character.base-mesh :as base-mesh]
            [character.blendshape :as blendshape]
            [character.hair :as hair]
            [character.body :as body]
            [character.params :as params]))

(def material-ids #{:skin :eye-white :iris :pupil :lip :eyebrow :hair :clothing :eyelash})

(defn generate-character
  "Generate a complete character mesh from a CharacterDef map. Returns
  `{:parts [MeshPart ...] :skeleton Skeleton :blendshape-targets [...]}`."
  [def]
  (let [{:keys [vertices indices]} (base-mesh/generate-head 48 64)
        head-verts (-> vertices
                        (blendshape/apply-face-shape (:face def))
                        (blendshape/apply-eye-shape (:eyes def))
                        (blendshape/apply-nose-shape (:nose def))
                        (blendshape/apply-mouth-shape (:mouth def)))
        head-verts (base-mesh/laplacian-smooth head-verts indices 2 0.2)
        head-normals (base-mesh/compute-normals head-verts indices)
        head-uvs (base-mesh/frontal-uv head-verts)
        head-vertices (vec (map-indexed (fn [i pos] {:position pos :normal (nth head-normals i) :uv (nth head-uvs i)}) head-verts))
        eye-parts (base-mesh/generate-eyes (:eyes def))
        eyebrow-parts (base-mesh/generate-eyebrows (:brows def))
        hair-part (hair/generate-hair (:hair def))
        skeleton (body/generate-humanoid-skeleton (:height (:body def)))
        body-part (body/skin-body (body/generate-body (:body def) (:bones skeleton)) (:bones skeleton))
        clothing-part (body/generate-clothing (:clothing def) (:body def))
        expression-targets (blendshape/generate-arkit-targets head-verts)
        head-part {:name "head" :vertices head-vertices :indices indices :material :skin}
        parts (into [head-part] (concat eye-parts eyebrow-parts))
        parts (conj parts hair-part body-part clothing-part)]
    {:parts parts :skeleton skeleton :blendshape-targets expression-targets}))
