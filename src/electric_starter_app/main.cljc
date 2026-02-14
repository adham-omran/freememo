(ns electric-starter-app.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [electric-starter-app.settings-page :refer [SettingsPage]]
            [electric-starter-app.pdf-page :refer [PdfPage]]
            [electric-starter-app.ocr-page :refer [OcrPage]]
            [electric-starter-app.cards-page :refer [CardsPage]]))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body] ; DOM nodes will mount under this one
      (dom/div ; mandatory wrapper div to ensure node ordering - https://github.com/hyperfiddle/electric/issues/74
        (dom/h1 (dom/text "Card Maker"))

        ;; Settings section
        (dom/div
          (SettingsPage))

        (dom/hr)  ; Visual separator

        ;; PDF Documents section
        (dom/div
          (PdfPage))

        (dom/hr)  ; Visual separator

        ;; OCR Text Extraction section
        (dom/div
          (OcrPage))

        (dom/hr)  ; Visual separator

        ;; Flashcard Generation section
        (dom/div
          (CardsPage))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request
