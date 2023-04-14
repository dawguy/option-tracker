(ns option-tracker.core
  (:require [net.cgrand.enlive-html :as html]
            [etaoin.api :as e]
            [clojure.data.csv :as csv]
            )
  (:import (java.io ByteArrayInputStream)
           ))

(defn data-dir [] "/tmp/OptionsData/")

(defn save-data [stock-name data date type] "TMUS (:calls {...}) call"
  (let [today-date (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (java.util.Date.))
        file-name (str (#'data-dir) stock-name "/" today-date "/" stock-name "-" date "-" type ".csv")]
    (clojure.java.io/make-parents file-name)
    (with-open [w (clojure.java.io/writer file-name)]
      (csv/write-csv w data)
      )))

; https://stackoverflow.com/questions/38283891/how-to-wrap-a-string-in-an-input-stream
(defn string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (ByteArrayInputStream.))))

(def a-page (atom {}))
(defn get-html [driver url]
  (prn "Getting data from " url)
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
   :date (clojure.string/replace (clojure.string/replace (html/text option) " " "-") "," "")})
(defn data->csv-format [d]
  (conj (map (fn [row] (map second row)) d) (vec (map #(str (symbol (first %))) (first d)))))
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
(defn save-options [stock-name]
  (let [pages (get-pages stock-name)
        m-date (reduce #(assoc %1 (:value %2) (:date %2)) {} (:available-dates (second (first pages))))]
    (loop [[p & rem] pages]
      (prn (first p))
      (if (nil? p)
        true
        (let [page-data (second p)]
          (save-data stock-name (data->csv-format (:calls page-data)) (get m-date (first p)) "calls")
          (save-data stock-name (data->csv-format (:puts page-data)) (get m-date (first p)) "puts")
          (recur rem))))))

(comment ""
  (save-options "BBBY")
  (save-options "GME")
  (save-options "T")
  (save-options "TMUS")
  (save-options "VZ")
,)
