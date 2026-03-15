(ns electric-starter-app.login-page
  "Login/signup full-page gate."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

(e/defn LoginPage [auth-error]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                          :height "100vh" :background "var(--color-bg-page)"}})
      (dom/div
        (dom/props {:style {:background "var(--color-bg-card)" :padding "40px" :border-radius "var(--radius-lg)"
                            :box-shadow "0 8px 30px rgba(0,0,0,0.12)" :width "360px"}})
        (dom/h1
          (dom/props {:style {:text-align "center" :margin-bottom "24px" :color "#333"}})
          (dom/text "FreeMemo"))

        ;; Error message
        (when auth-error
          (dom/div
            (dom/props {:style {:background "#fee" :color "var(--color-danger)" :padding "10px" :border-radius "var(--radius-sm)"
                                :margin-bottom "var(--sp-4)" :font-size "14px"}})
            (dom/text auth-error)))

        ;; Google sign-in — primary action
        (dom/a
          (dom/props {:href "/auth/google"
                      :class "btn btn-primary" :style {:display "block" :text-align "center" :padding "12px 16px"
                              :text-decoration "none" :font-size "16px"}})
          (dom/text "Sign in with Google"))

        ;; Divider
        (dom/div
          (dom/props {:style {:text-align "center" :margin "20px 0 16px" :color "var(--color-text-hint)" :font-size "13px"}})
          (dom/text "— or sign in with username —"))

        ;; Collapsible username/password section
        (let [!expanded (atom false)
              expanded (e/watch !expanded)]

          (dom/div
            (dom/props {:style {:cursor "pointer" :font-size "14px" :color "var(--color-text-secondary)"
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
                  (dom/props {:style {:margin-bottom "var(--sp-4)"}})
                  (dom/label
                    (dom/props {:class "label"})
                    (dom/text "Username"))
                  (dom/input
                    (dom/props {:type "text" :name "username" :required true
                                :class "input input-full" :style {:padding "10px"}})))

                (dom/div
                  (dom/props {:style {:margin-bottom "var(--sp-6)"}})
                  (dom/label
                    (dom/props {:class "label"})
                    (dom/text "Password"))
                  (dom/input
                    (dom/props {:type "password" :name "password" :required true
                                :class "input input-full" :style {:padding "10px"}})))

                (dom/button
                  (dom/props {:type "submit"
                              :class "btn" :style {:width "100%" :padding "10px" :background "#555" :color "white"
                                      :font-size "16px"}})
                  (dom/text (if (= mode :login) "Log In" "Sign Up"))))

              ;; Toggle link
              (dom/div
                (dom/props {:style {:text-align "center" :margin-top "var(--sp-3)" :font-size "14px" :color "var(--color-text-secondary)"}})
                (dom/text (if (= mode :login) "Don't have an account? " "Already have an account? "))
                (dom/a
                  (dom/props {:href "#"
                              :style {:color "var(--color-primary)" :text-decoration "none" :font-weight "500"}})
                  (dom/text (if (= mode :login) "Sign up" "Log in"))
                  (dom/On "click" (fn [e]
                                    (.preventDefault e)
                                    (reset! !mode (if (= mode :login) :signup :login)))
                    nil))))))))))
