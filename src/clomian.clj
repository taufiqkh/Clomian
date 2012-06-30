(ns clomian
  (:require clojure.java.io)
  (:use [incanter core charts]
        carrit.region-file
        [clojure.tools.logging :only [info]])
  (:gen-class))

;(set! *warn-on-reflection* true)

(def types {0 "Air"
            4 "Cobblestone"
            8 "Water"
            12 "Sand"
            16 "Coal ore"
            20 "Glass"
            24 "Lime cloth"
            28 "Blue cloth"
            32 "Magenta cloth"
            36 "White cloth"
            40 "Red mushroom"
            44 "Step"
            48 "Mossy cobblestone"
            52 "Mob spawner"
            56 "Diamond ore"
            60 "Soil"
            64 "Wooden door"
            68 "Wall sign"
            72 "Wooden pressure plate"
            76 "Redstone torch (on)"
            80 "Snow block"
            84 "Jukebox"
            88 "Slow sand"
            3 "Dirt"
            7 "Bedrock"
            11 "Stationary lava"
            15 "Iron ore"
            19 "Sponge"
            23 "Yellow cloth"
            27 "Cyan cloth"
            31 "Violet cloth"
            35 "Gray / white cloth"
            39 "Brown mushroom"
            43 "Double step"
            47 "Bookshelf"
            51 "Fire"
            55 "Redstone wire"
            59 "Crops"
            63 "Sign post"
            67 "Cobblestone stairs"
            71 "Iron door"
            75 "Redstone torch (off)"
            79 "Ice"
            83 "Reed"
            87 "Bloodstone"
            91 "Jack-o-lantern"
            2 "Grass"
            6 "Sapling"
            10 "Lava"
            14 "Gold ore"
            18 "Leaves"
            22 "Orange cloth"
            26 "Aqua green cloth"
            30 "Indigo cloth"
            34 "Black cloth"
            38 "Red rose"
            42 "Iron block"
            46 "TNT"
            50 "Torch"
            54 "Chest"
            58 "Workbench"
            62 "Burning furnace"
            66 "Minecart tracks"
            70 "Stone pressure plate"
            74 "Glowing redstone ore"
            78 "Snow"
            82 "Clay"
            86 "Pumpkin"
            90 "Portal"
            1 "Stone"
            5 "Wood"
            9 "Stationary water"
            13 "Gravel"
            17 "Log"
            21 "Red cloth"
            25 "Green cloth"
            29 "Purple cloth"
            33 "Pink cloth"
            37 "Yellow flower"
            41 "Gold block"
            45 "Brick"
            49 "Obsidian"
            53 "Wooden stairs"
            57 "Diamond block"
            61 "Furnace"
            65 "Ladder"
            69 "Lever"
            73 "Redstone ore"
            77 "Stone button"
            81 "Cactus"
            85 "Fence"
            89 "Lightstone"})

(def graph-height-limit 128)

(def chunks-per-section 16)
(def blocks-per-chunk-x 16)
(def blocks-per-chunk-y 16)
(def blocks-per-chunk-z 16)
(def blocks-per-chunk-layer (* blocks-per-chunk-x blocks-per-chunk-z))

(def section-max 15)

(defn aconcat [& ars]
  (let [offset (reductions + (map alength ars))
        total-length (last offset)
        far (java.util.Arrays/copyOf (first ars) total-length)]
    (doseq [[ar off] (map vector (next ars) offset)]
      (System/arraycopy ar 0 far off (alength ar)))
    far))

(defn block-nbts [region-file-map]
  (let [files (vals region-file-map)
        regions (doall (map read-region-file files))
        first-region (first regions)]
    (clojure.tools.logging/info (create-file-descriptor (.getName (first files))) (first files))
    (clojure.tools.logging/info (:filename first-region) (:x first-region) (:z first-region))
    (let [chunks (mapcat :chunks regions)
          ;chunks (mapcat #(:chunks %) regions)
          n (info (count chunks))
          maybe-nil (map #(let [[[x z] chunk] %] (:nbt chunk)) chunks)
          chunk-nbts (filter #(not (nil? %)) maybe-nil)
          chunk-blocks (mapcat #(get-in % [:data "Level" :data "Sections" :data]) chunk-nbts)]
      (assert (and (not (nil? chunks)) (not (empty? chunks))))
      (assert (not (empty? chunk-nbts)))
      (doseq [item chunk-nbts]
        (assert (not (nil? item))))
      (assert (not (empty? chunk-nbts)))
      (assert (and (not (nil? chunk-blocks)) (not (empty? chunk-blocks))))
      (doseq [item chunk-blocks] (assert (not (nil? item))))
      chunk-blocks)))

(defn y-ordered-blocks [block-nbts]
  (let [section-y-range (range 0 (inc section-max))
        ordered-blocks (zipmap section-y-range (map (fn [_] []) section-y-range))]
        (reduce (fn [ordered-result block-nbt]
                  (let [y (get-in block-nbt ["Y" :data])
                        blocks (get-in block-nbt ["Blocks" :data])]
                    (assoc ordered-result y (conj (ordered-result y) block-nbt))))
                ordered-blocks
                block-nbts)))

(defn coord-to-block-idx [x y z]
  "Translates an x/y/z coordinate to a block index"
  )

(defn copy-block [block-array y output output-start-idx]
  (loop [output-idx output-start-idx
         x 0
         z 0]
    (if (= output-idx (+ output-start-idx blocks-per-chunk-layer))
      output
      (let [block-idx (+ x (* z blocks-per-chunk-x) (* y blocks-per-chunk-layer))]
        (aset output output-idx (aget block-array block-idx))
        (recur (inc output-idx) (inc x) (inc z))))))

; TODO: Investigate performance improvement by checking frequencies over original arrays instead of copying
(defn get-layer [y blocks]
  (let [section-y (quot y chunks-per-section)
        size (* (count (blocks y)) blocks-per-chunk-layer)
        output (byte-array size)]
    ; Go through each block array, copying it to output
    (assert (= (count (blocks y)) (count (range 0 size blocks-per-chunk-layer))))
    (doseq [[output-start-idx block-array] (map vector (range 0 size blocks-per-chunk-layer) (blocks y))]
      (copy-block block-array y output output-start-idx))))

(defn afrequencies
  [^bytes a]
  (persistent!
   (areduce a idx counts (transient {})
            (let [x (aget a idx)]
              (assoc! counts x (inc (get counts x 0)))))))

(defn freqs [blocks]
  (let [layers (map #(get-layer % blocks) (range graph-height-limit))]
    (pmap afrequencies layers)))

(defn plotfn [freqs btype layer]
  (get (nth freqs layer) (byte btype) 0))

(defn -main [path & options]
  (let [options (set (map #(Integer. ^String %) options))
        fr (time (let [region-files (:region-files (save-dir-files path))
                       block-nbts (block-nbts region-files)]
                   (y-ordered-blocks block-nbts)))
        fr (time (freqs fr))        
        canvas (time (-> (reduce #(add-function %1 (partial plotfn fr (key %2)) 0 graph-height-limit
                                          :series-label (val %2))
                           (xy-plot [] []
                                    :x-label "Layer"
                                    :y-label "Blocks"
                                    :legend true)
                            (select-keys types options))))]
    ;(slider #(set-y-range canvas 0 %) (range 0 500))
    (view canvas)))
    ;(save canvas "graph.png")
    ;(save (set-y-range canvas 0 50) "graph-low.png")))
