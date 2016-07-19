
(defclass Empty []
  []
  (defn draw [this] "   -----  "))

(defclass Down [] 
  [[--init--
  (fn [self x]
    (setv self.down x)
    None)]]

  (defn draw [this] (format "   %2d\\--  " (.down this))))

(defclass Across []
  [[--init--
  (fn [self x]
    (setv self.across x)
    None)]]

  (defn draw [this] (format "   --\\%2d  " (:across this))))

(defclass DownAcross []
  [[--init--
  (fn [self x y]
    (setv self.down x)
    (setv self.across y)
    None)]]

  (defn draw [this] (format "   %2d\\%2d  " (:down this) (:across this))))

(defclass Value []
  [[--init--
  (fn [self x]
   (setv self.values x)
   None)]]

  (defn draw [this]
    (let [values (.values this)]
      (if (= 1 (len values))
        (str "     " (first values) "    ")
        (apply str " " (->> (range 1 10) 
                            (map (fn [x] (if (contains? values x) x ".")))))))))

(defn draw-row [row]
  (str (apply str (map draw row)) "\n"))

(defn draw-grid [grid]
  (apply str (map draw-row grid)))

(def v (Value (set [1 2 3 4 5 6 7 8 9])))
(def e (Empty))
(defn d [n] (Down n))
(defn a [n] (Across n))
(defn da [d a] (DownAcross d a))

(defn all-different [nums]
  (= (count nums) (count (set nums))))

(defn permute [vs target so-far]
  (if (>= target 1)
    (if (= (count so-far) (dec (count vs)))
      [(conj so-far target)]
      (->> (get vs (count so-far))
           :values
           (mapcat (fn [x] (permute vs (- target x) (conj so-far x))))
           (into [])))
    []))

(defn permute-all [vs total]
  (permute vs total []))

(defn is-possible? [cell n]
  (contains? (:values cell) n))

(defn cell? [v]
  (instance? kakuro.core.Cell v))

(defn transpose [m]
  (apply (partial mapv vector) m))

(defn solve-step [cells total]
  (let [final (dec (count cells))]
       [perms (->> (permute-all cells total)
         (filter (fn [x] (is-possible? (get cells final) (get x final))))
         (filter all-different))]
    (->> perms
         transpose
         (map (fn [x] (Value (set x)))))))

(defn solve-pair [f pair]
  (let [[nvs vs] pair
        target (f (last nvs))]
    (if (seq vs)
      (concat nvs (solve-step (into [] vs) target))
      nvs)))

(defn gather-values [line]
  (partition-by (fn [x] (= (type x) (type v)) line)))

(defn pair-target-with-values [line]
  (partition-all 2 (gather-values line)))

(defn solve-line [line pair-solver]
  (let [pairs (pair-target-with-values line)]
    (into [] (mapcat pair-solver pairs))))

(defn solve-row [row]
  (solve-line row (fn [x] (solve-pair :across x))))

(defn solve-column [column]
  (solve-line column (fn [x] (solve-pair :down x))))

(defn solve-grid [grid]
  (->> grid
       (mapv solve-row)
       transpose
       (mapv solve-column)
       transpose))

(defn solver [grid]
  (let [g (solve-grid grid)]
    (if (= g grid)
      g
      (solver g))))

