(ns gilded-rose.core-spec
(:require [speclj.core :refer :all]
          [gilded-rose.core :refer :all]))

(defn test-item
  "Construct a mock item with the desired traits"
  [sell-in quality & traits]
  (apply with-traits (item "Test item" sell-in quality) traits))

(defn quality-diff
  "Return the quality difference when updating just this item"
  [item]
  (let [old-quality (:quality item)
        new-quality (:quality (first (update-quality [item])))]
    (- new-quality old-quality)))

(defn as-expired [item]
  (assoc item :sell-in -1))

(defn as-conjured [item]
  (update item :traits conj conjured))

(let [hq-item  (test-item 10 90 standard)
      lq-item  (test-item 10 -5 standard)
      sulfuras (test-item 10 10 permanent)]
  (describe "item quality should be within limits"
            (it "should not be increased beyond 50"
                (should (= -40 (quality-diff hq-item))))
            (it "but Sulfuras can go over 50"
                (should (zero? (quality-diff sulfuras))))
            (it "most items can't go below zero"
                (should (= 5 (quality-diff lq-item))))))

(let [item (test-item 10 5 standard)]
  (describe "standard items"
            (it "quality reduced by 1 (base)"
                (should (= -1 (quality-diff item))))
            (it "quality reduced by 2 (expired)"
                (should (= -2 (quality-diff (as-expired item)))))
            (it "quality reduced by 2 (conjured)"
                (should (= -2 (quality-diff (as-conjured item)))))
            (it "quality reduced by 4 (conjured, expired)"
                (should (= -4 (quality-diff (as-conjured (as-expired item))))))))

(let [item (test-item 10 5 accruing standard)]
  (describe "accruing items"
            (it "quality increases daily"
                (should (= 1 (quality-diff item))))
            (it "quality increases by 1 (expired)"
                (should (= 1 (quality-diff (as-expired item)))))
            (it "quality increases by 1 (conjured)"
                (should (= 1 (quality-diff (as-conjured item)))))
            (it "quality increases by 1 (conjured, expired)"
                (should (= 1 (quality-diff (as-expired (as-conjured item))))))))

(let [item (test-item 10 5 permanent)]
  (describe "permanent items"
            (it "don't decrease in quality (base)"
                (should (zero? (quality-diff item))))
            (it "don't decrease in quality (expired)"
                (should (zero? (quality-diff (as-expired item)))))
            (it "don't decrease in quality (conjured)"
                (should (zero? (quality-diff (as-conjured item)))))
            (it "don't decrease in quality (conjured)"
                (should (zero? (quality-diff (as-conjured (as-expired item))))))))

(let [item            (test-item 15 20 ticketmaster standard)
      item-danger     (assoc item :sell-in 10)
      item-d-d-danger (assoc item :sell-in 5)]
  (describe "backstage passes"
            (it "increases normally far from the concert"
                (should (= 1 (quality-diff item))))
            (it "increases by 2 with <= 10 days left"
                (should (= 2 (quality-diff item-danger))))
            (it "increases by 2 with <= 10 days left (conjured)"
                (should (= 2 (quality-diff (as-conjured item-danger)))))
            (it "increases by 3 with <= 5 days left"
                (should (= 3 (quality-diff item-d-d-danger))))
            (it "increases by 3 with <= 5 days left (conjured)"
                (should (= 3 (quality-diff (as-conjured item-d-d-danger)))))
            (it "is worthless after the concert"
                (should (= -20 (quality-diff (as-expired item)))))))
