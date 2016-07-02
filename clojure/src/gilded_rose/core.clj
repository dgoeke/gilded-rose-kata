(ns gilded-rose.core)

(def max-quality 50)  ; calling (limit-quality ...) applies these
(def min-quality 0)

(defprotocol ItemTrait
  "Item traits may modify the rate of quality and sell-in decay."
  (adjust-quality [this item q-delta] "Given a quality adjustment, return a new one.")
  (adjust-sell-in [this item sell-in-delta] "Given a sell-in adjustment, return a new one."))

(defn with-traits
  "Apply the given traits to this item"
  [item & traits]
  (assoc item :traits traits))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(defn compose-item-traits
  "Convert an items seq of traits into a function by composing them all together"
  [item f]
  (if-let [traits (:traits item)]
    (->> traits
         (map (fn [trait] (partial f trait item)))
         (apply comp))
    identity))

(defn adjust-item
  "Modify an item by running `trait-fn` in all traits and adding the reuslt to `field`"
  [item field trait-fn]
  (let [delta-fn (compose-item-traits item trait-fn)
        delta    (delta-fn 0)]
    (update item field (partial + delta))))

(defn adjust-item-quality
  "Run all `adjust-quality` traits and update the :quality field"
  [item]
  (adjust-item item :quality adjust-quality))

(defn adjust-item-sell-in
  "Run all `adjust-sell-in` traits and update the :sell-in field"
  [item]
  (adjust-item item :sell-in adjust-sell-in))

(defn update-quality [inventory]
  (->> inventory
       (map adjust-item-sell-in)
       (map adjust-item-quality)))

(defn limit-quality
  "Alter q-delta so it doesn't take quality out of bounds"
  [item q-delta]
  (let [quality     (:quality item)
        new-quality (max min-quality (min max-quality (+ quality q-delta)))]
    (- new-quality quality)))

;; Traits!
(def standard
  (reify ItemTrait
    (adjust-quality [_ item _]
      (limit-quality item (if (neg? (:sell-in item)) -2 -1)))
    (adjust-sell-in [_ _ _] -1)))

(def conjured
  (reify ItemTrait
    (adjust-quality [_ item delta]
      (limit-quality item (if (neg? delta) (* 2 delta) delta)))
    (adjust-sell-in [_ _ delta] delta)))

(def accruing
  (reify ItemTrait
    (adjust-quality [_ item _]
      (limit-quality item (if (>= (:quality item) max-quality) 0 1)))
    (adjust-sell-in [_ _ delta] delta)))

(def permanent
  (reify ItemTrait
    (adjust-quality [_ _ _] 0)
    (adjust-sell-in [_ _ _] 0)))

(def ticketmaster
  (reify ItemTrait
    (adjust-quality [_ item delta]
      (let [{:keys [sell-in quality]} item]
        (limit-quality item
         (cond (neg? sell-in)  (- 0 quality) ; lower back to zero
               (<= sell-in 5)  3
               (<= sell-in 10) 2
               :else           1))))

    (adjust-sell-in [_ _ delta] delta)))

(defn update-current-inventory[]
  (let [inventory [(with-traits (item "+5 Dexterity Vest" 10 20) standard)
                   (with-traits (item "Aged Brie" 2 0) accruing standard)
                   (with-traits (item "Elixir of the Mongoose" 5 7) standard)
                   (with-traits (item "Sulfuras, Hand Of Ragnaros" 0 80) permanent)
                   (with-traits (item "Backstage passes to a TAFKAL80ETC concert" 15 20)
                     ticketmaster standard)]]
    (update-quality inventory)))
