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

        (let [!mode (atom :login)
              mode (e/watch !mode)]

          ;; Error message
          (when auth-error
            (dom/div
              (dom/props {:style {:background "#fee" :color "#c00" :padding "10px" :border-radius "4px"
                                  :margin-bottom "16px" :font-size "14px"}})
              (dom/text auth-error)))

          ;; Form
          (dom/form
            (dom/props {:action (if (= mode :login) "/api/login" "/api/signup")
                        :method "post"})

            (dom/div
              (dom/props {:style {:margin-bottom "16px"}})
              (dom/label
                (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "14px"}})
                (dom/text "Username"))
              (dom/input
                (dom/props {:type "text" :name "username" :required true :autofocus true
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
                          :style {:width "100%" :padding "10px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "4px" :font-size "16px"
                                  :cursor "pointer" :font-weight "500"}})
              (dom/text (if (= mode :login) "Log In" "Sign Up"))))

          ;; Toggle link
          (dom/div
            (dom/props {:style {:text-align "center" :margin-top "16px" :font-size "14px" :color "#666"}})
            (dom/text (if (= mode :login) "Don't have an account? " "Already have an account? "))
            (dom/a
              (dom/props {:href "#"
                          :style {:color "#2563eb" :text-decoration "none" :font-weight "500"}})
              (dom/text (if (= mode :login) "Sign up" "Log in"))
              (dom/On "click" (fn [e]
                                (.preventDefault e)
                                (reset! !mode (if (= mode :login) :signup :login)))
                nil))))))))
