(ns option-tracker.core
  (:require [clj-http.client :as c]
            [net.cgrand.enlive-html :as html])
  (:import (java.io ByteArrayInputStream)))

(def stocks ["bbby" "gme" "tmus"])
(def stocks ["bbby"])
(def stock "BBBY")
(def date 1680220800)

; https://stackoverflow.com/questions/38283891/how-to-wrap-a-string-in-an-input-stream
(defn string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (ByteArrayInputStream.))))

(def a-page (atom {}))
(defn get-page
  ([stock] (get-page stock nil))
  ([stock date]
   (let [url (if (nil? date)
               (str "https://finance.yahoo.com/quote/" stock "/options?p=" stock)
               (str "https://finance.yahoo.com/quote/" stock "/options?date=" date))
         page (:body (c/get url {:headers {
                                           :accept                    "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
                                           :accept-encoding           "gzip, deflate, br"
                                           :accept-language           "en-US,en;q=0.9"
                                           :cache-control             "no-cache"
                                           :pragma                    "no-cache"
                                           ;:user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36"
                                           :upgrade-insecure-requests "1"
                                           }}))]
     (reset! a-page page)
     page)))

(def a-p (atom {}))
(defn get-and-parse
  ([stock] (get-and-parse stock nil))
  ([stock date]
   (let [p (html/html-resource (string->stream (get-page stock date)))]
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

(def a-pages (atom {}))
(defn get-pages [stock]
  (let [initial-page (page->data (get-and-parse stock))
        pages (reduce (fn [acc d] (assoc acc d (page->data (get-and-parse stock d))))
                {}
                (:available-dates initial-page))]
    (reset! a-pages pages)
    pages))

(get-pages "BBBY")
@a-pages
(page->data (get-and-parse "BBBY"))
(page->data @a-p)
(html/select @a-p [:div.controls :select])