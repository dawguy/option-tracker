(ns option-tracker.core
  (:require [net.cgrand.enlive-html :as html]
            [etaoin.api :as e]
            [etaoin.keys :as k])
  (:import (java.io ByteArrayInputStream)))

; https://stackoverflow.com/questions/38283891/how-to-wrap-a-string-in-an-input-stream
(defn string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (ByteArrayInputStream.))))

(def a-page (atom {}))
(defn get-html [driver url]
  (if (e/running? driver)
    (do
      (e/go driver url)
      (e/wait-visible driver {:css "body"})
      (e/get-element-inner-html driver {:css "body"}))
    nil))
(defn get-page
  ([driver stock] (get-page driver stock nil))
  ([driver stock date]
   (let [url (if (nil? date)
               (str "https://finance.yahoo.com/quote/" stock "/options?p=" stock)
               (str "https://finance.yahoo.com/quote/" stock "/options?date=" date))
         page (get-html driver url)]
     (reset! a-page page)
     page)))

(def a-p (atom {}))
(defn get-and-parse
  ([driver stock] (get-and-parse driver stock nil))
  ([driver stock date]
   (let [p (html/html-resource (string->stream (get-page driver stock date)))]
     (reset! a-p p)
     p)))

(def column-layout [:contract-name
                    :last-trade-date
                    :strike
                    :last-price
                    :bid
                    :ask
                    :change
                    :percent-change
                    :volume
                    :open-interest
                    :implied-volatility])
(defn option-tr->map [tr]
  (let [tds (html/select tr [:td])]
    (loop [[c & rem] column-layout
           m {}]
      (if (nil? c)
        m
        (recur rem (assoc m c (html/text (nth tds (- (count tds) (inc (count rem))))))))
    )))
(defn date-option->map [option]
  {:value (first (html/attr-values option :value))
   :date (html/text option)})
(defn page->data [p]
  (let [call-trs (html/select  p [:table.calls :tbody :tr])
        put-trs (html/select  p [:table.puts :tbody :tr])
        date-options (html/select p [:div.controls :option])
        ]
    {
     :calls (map option-tr->map call-trs)
     :puts (map option-tr->map put-trs)
     :available-dates (map date-option->map date-options)
     }
    ))

(def a-search-stock (atom {}))
(def a-pages (atom {}))
(defn get-pages [stock]
  (e/with-chrome-headless driver
                          (let [initial-page (page->data (get-and-parse driver stock))
                                pages (reduce (fn [acc d] (assoc acc d (page->data (get-and-parse driver stock d))))
                                              {}
                                              (map :value (:available-dates initial-page)))]
                            (reset! a-pages pages)
                            (reset! a-search-stock stock)
                            pages)))

(get-pages "TMUS")
(prn (str "Loaded options data for " @a-search-stock " " (keys @a-pages)))
(page->data @a-p)