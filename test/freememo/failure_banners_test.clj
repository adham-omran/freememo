(ns freememo.failure-banners-test
  "The pure half of the banner strip (plans/remove-toasts.md 1.2, 1.3): which
   banners exist for a set of failures and account state. No database, no
   browser."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.failure-banners :as fb]))

(defn- failure
  [id & {:as more}]
  (merge {:id id :type "action-failure" :label "Action failed" :what "Delete card"
          :error "boom" :error-kind nil :subject-id nil :retry? false}
    more))

(deftest two-failures-give-two-banners
  (let [banners (fb/collapse-failures [(failure 2) (failure 1)])]
    (is (= ["failure-2" "failure-1"] (mapv :key banners)))
    (is (= [[2] [1]] (mapv :ids banners)))))

(deftest a-failure-banner-carries-its-actions
  (let [[b] (fb/collapse-failures
              [(failure 7 :subject-id 99 :retry? true :error "Network error")])]
    (is (= "Delete card failed: Network error" (:message b)))
    (is (= 99 (:subject-id b)))
    (is (true? (:retry? b)))
    (is (= :failures (:dismiss b)))
    (is (not (:topup? b)))))

(deftest a-run-is-named-by-its-label-and-an-action-by-its-what
  (is (= "Generating cards failed: x"
        (fb/failure-message (failure 1 :type "card-gen" :label "Generating cards"
                              :what "generating" :error "x"))))
  (is (= "Delete card failed: x" (fb/failure-message (failure 1 :error "x"))))
  (is (= "Action failed." (fb/failure-message (failure 1 :what nil :error "")))))

(deftest twelve-credit-failures-give-one-banner
  (let [rows (mapv #(failure % :error-kind "insufficient-credits") (range 12 0 -1))
        banners (fb/collapse-failures rows)]
    (is (= 1 (count banners)))
    (is (= "12 actions need credits." (:message (first banners))))
    (is (= (mapv :id rows) (:ids (first banners))) "Dismiss acknowledges all twelve")
    (is (true? (:topup? (first banners))))
    (is (nil? (:subject-id (first banners))) "no Open for many subjects")
    (is (false? (:retry? (first banners))) "no Retry for many commands")))

(deftest one-failure-of-a-kind-stays-single-and-shows-its-action
  (let [[b] (fb/collapse-failures [(failure 1 :error-kind "insufficient-credits" :subject-id 5)])]
    (is (= "failure-1" (:key b)))
    (is (true? (:topup? b)))
    (is (= 5 (:subject-id b)))))

(deftest collapsing-keeps-the-order-and-the-other-failures
  (let [banners (fb/collapse-failures
                  [(failure 5 :error-kind "insufficient-credits")
                   (failure 4)
                   (failure 3 :error-kind "insufficient-credits")
                   (failure 2 :error-kind "other-kind")
                   (failure 1)])]
    (is (= ["kind-insufficient-credits" "failure-4" "failure-2" "failure-1"]
          (mapv :key banners)))
    (testing "every failure id lands in exactly one banner"
      (is (= [1 2 3 4 5] (sort (mapcat :ids banners)))))))

(deftest the-pause-banner-cannot-be-dismissed
  (let [[b :as banners] (fb/account-banners {:paused {:until "16 November 2026"}})]
    (is (= 1 (count banners)))
    (is (= "pause" (:key b)))
    (is (= "Video playback is paused. Your videos are kept until 16 November 2026."
          (:message b)))
    (is (nil? (:dismiss b)))
    (is (true? (:topup? b)))))

(deftest the-removal-notice-can-be-dismissed
  (let [[b] (fb/account-banners {:removal {:count 3 :on "2 November 2026"}})]
    (is (= "3 videos were removed on 2 November 2026." (:message b)))
    (is (= :removal (:dismiss b))))
  (is (= "1 video was removed on 2 November 2026."
        (:message (first (fb/account-banners {:removal {:count 1 :on "2 November 2026"}}))))))

(deftest account-banners-come-before-failure-banners
  (let [all (into (fb/account-banners {:paused {:until "x"} :removal {:count 2 :on "y"}})
              (fb/collapse-failures [(failure 1)]))]
    (is (= ["pause" "removal" "failure-1"] (mapv :key all)))))

(deftest update-banners-say-what-the-student-must-do
  (testing "a newer version gives \"Update available\" with the installer link"
    (let [[b] (fb/update-banners {:update-available {:version "2027.1.1" :url "https://x/app"}})]
      (is (= "Update available" (:message b)))
      (is (= "https://x/app" (:href b)))
      (is (nil? (:dismiss b)))))
  (testing "a refused Sync Protocol gives one banner, \"Update to keep syncing\""
    (let [banners (fb/update-banners {:update-available {:version "2027.1.1" :url "https://x/app"}
                                      :update-required? true})]
      (is (= ["Update to keep syncing"] (mapv :message banners)))
      (is (= "https://x/app" (:href (first banners))))))
  (testing "nothing to say gives no banner"
    (is (empty? (fb/update-banners {:update-available nil :update-required? false})))
    (is (empty? (fb/update-banners nil)))))

(deftest no-state-gives-no-banner
  (is (empty? (fb/account-banners nil)))
  (is (empty? (fb/account-banners {:paused nil :removal nil})))
  (is (empty? (fb/collapse-failures []))))
