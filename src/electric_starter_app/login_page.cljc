(ns electric-starter-app.login-page
  "Login/signup full-page gate."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

(e/defn LoginPage [auth-error]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                          :height "100vh" :background "#f5f5f5"}})
      (dom/div
        (dom/props {:style {:background "white" :padding "40px" :border-radius "8px"
                            :box-shadow "0 2px 10px rgba(0,0,0,0.1)" :width "360px"}})
        (dom/h1
          (dom/props {:style {:text-align "center" :margin-bottom "24px" :color "#333"}})
          (dom/text "Card Maker"))

        ;; Error message
        (when auth-error
          (dom/div
            (dom/props {:style {:background "#fee" :color "#c00" :padding "10px" :border-radius "4px"
                                :margin-bottom "16px" :font-size "14px"}})
            (dom/text auth-error)))

        ;; Google sign-in — primary action
        (dom/a
          (dom/props {:href "/auth/google"
                      :style {:display "block" :text-align "center" :padding "12px 16px"
                              :border "none" :border-radius "4px"
                              :text-decoration "none" :color "white" :font-size "16px"
                              :font-weight "500" :background "#2563eb" :cursor "pointer"}})
          (dom/text "Sign in with Google"))

        ;; Divider
        (dom/div
          (dom/props {:style {:text-align "center" :margin "20px 0 16px" :color "#999" :font-size "13px"}})
          (dom/text "— or sign in with email —"))

        ;; Collapsible email/password section
        (let [!expanded (atom false)
              expanded (e/watch !expanded)]

          (dom/div
            (dom/props {:style {:cursor "pointer" :font-size "14px" :color "#666"
                                :font-weight "500" :user-select "none" :padding "4px 0"}})
            (dom/text (if expanded "▾ Email & password" "▸ Email & password"))
            (dom/On "click" (fn [e] (.preventDefault e) (swap! !expanded not)) nil))

          (when expanded
            (let [!mode (atom :login)
                  mode (e/watch !mode)]

              ;; Form
              (dom/form
                (dom/props {:action (if (= mode :login) "/api/login" "/api/signup")
                            :method "post"
                            :style {:margin-top "12px"}})

                (dom/div
                  (dom/props {:style {:margin-bottom "16px"}})
                  (dom/label
                    (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "14px"}})
                    (dom/text "Username"))
                  (dom/input
                    (dom/props {:type "text" :name "username" :required true
                                :style {:width "100%" :padding "10px" :border "1px solid #ccc"
                                        :border-radius "4px" :font-size "14px" :box-sizing "border-box"}})))

                (dom/div
                  (dom/props {:style {:margin-bottom "24px"}})
                  (dom/label
                    (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "14px"}})
                    (dom/text "Password"))
                  (dom/input
                    (dom/props {:type "password" :name "password" :required true
                                :style {:width "100%" :padding "10px" :border "1px solid #ccc"
                                        :border-radius "4px" :font-size "14px" :box-sizing "border-box"}})))

                (dom/button
                  (dom/props {:type "submit"
                              :style {:width "100%" :padding "10px" :background "#555" :color "white"
                                      :border "none" :border-radius "4px" :font-size "16px"
                                      :cursor "pointer" :font-weight "500"}})
                  (dom/text (if (= mode :login) "Log In" "Sign Up"))))

              ;; Toggle link
              (dom/div
                (dom/props {:style {:text-align "center" :margin-top "12px" :font-size "14px" :color "#666"}})
                (dom/text (if (= mode :login) "Don't have an account? " "Already have an account? "))
                (dom/a
                  (dom/props {:href "#"
                              :style {:color "#2563eb" :text-decoration "none" :font-weight "500"}})
                  (dom/text (if (= mode :login) "Sign up" "Log in"))
                  (dom/On "click" (fn [e]
                                    (.preventDefault e)
                                    (reset! !mode (if (= mode :login) :signup :login)))
                    nil))))))))))
