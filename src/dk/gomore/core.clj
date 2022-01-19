(ns dk.gomore.core
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.instant :as inst]
            [potpuri.core :as pt])
  (:gen-class)
  (:import (clojure.lang ExceptionInfo)
           (java.util Date)
           (java.text SimpleDateFormat)))


(defn date? [d]
  (try (inst/read-instant-date d)
       (catch Exception _ false)))

(def integer-regex #"\d+")

(def integer-string? (partial re-matches integer-regex))

(def city-regex #"[A-Za-z]+")

(def city-string? (partial re-matches city-regex))

(s/def ::from-city city-string?)
(s/def ::to-city city-string?)
(s/def ::date date?)
(s/def ::number-of-seats integer-string?)
(s/def ::minimum-free-seats integer-string?)

(s/def ::create-args (s/cat :from-city ::from-city
                            :to-city ::to-city
                            :date ::date
                            :number-of-seats ::number-of-seats))

(s/def ::return-args (s/cat :date ::date))

(s/def ::search-args (s/cat :from-city (s/? ::from-city)
                            :to-city (s/? ::to-city)
                            :from-date (s/? ::date)
                            :to-date (s/? ::date)
                            :minimum-free-seats (s/? ::minimum-free-seats)))

(defn print-help-message! []
  (doseq [msg ["Usage: "
               "C from-city to-city date number-of-seats"
               "R date"
               "S [from-city [to-city]] [from-date [to-date]] [minimum-free-seats]"]]
    (println msg)))

(defn parse-args [args spec]
  (let [conformed-args (s/conform spec args)]
    (when (= :clojure.spec.alpha/invalid conformed-args)
      (throw (ex-info (str "Invalid arguments for command: " args)
                      {:reason (s/explain-str spec args)})))
    conformed-args))

(defn normalize-create-args [{:keys [date number-of-seats] :as create-args}]
  (cond-> create-args
    date (update :date inst/read-instant-date)
    number-of-seats (update :number-of-seats #(Integer/parseInt %))))

(defn normalize-return-args [return-args]
  (update return-args :date inst/read-instant-date))

(defn normalize-search-args [{:keys [from-date to-date minimum-free-seats] :as search-args}]
  (cond-> search-args
    from-date (update :from-date inst/read-instant-date)
    to-date (update :to-date inst/read-instant-date)
    minimum-free-seats (update :minimum-free-seats #(Integer/parseInt %))))

(defn print-matched-rides! [rides]
  (println "==>")
  (doseq [{:keys [from-city to-city date number-of-seats]} rides]
    (let [date-formatter (SimpleDateFormat. "yyyy-MM-dd")]
      (println (str/join " " [from-city to-city
                              (.format ^SimpleDateFormat date-formatter date)
                              number-of-seats])))))

(defn prompt! []
  (try
    (loop [rides [] last-ride nil]
      (print "> ")
      (.flush *out*)
      (let [[command & args] (str/split (read-line) #" ")]
        (case command
          "C" (let [created-ride (normalize-create-args (parse-args args ::create-args))]
                (recur (conj rides created-ride) created-ride))
          "R" (let [{:keys [date]} (normalize-return-args (parse-args args ::return-args))
                    {:keys [from-city to-city number-of-seats]} last-ride
                    return-ride {:number-of-seats number-of-seats
                                 :date            date
                                 :to-city         from-city
                                 :from-city       to-city}]
                (recur (conj rides return-ride) return-ride))
          "S" (let [{:keys [from-city to-city from-date to-date minimum-free-seats]}
                    (normalize-search-args (parse-args args ::search-args))

                    tf
                    (cond->> identity
                             from-city (comp (filter (pt/where-fn {:from-city from-city})))
                             to-city (comp (filter (pt/where-fn {:to-city to-city})))
                             from-date (comp (filter (fn [{:keys [date]}]
                                                       (<= (-> ^Date from-date .toInstant .getEpochSecond)
                                                           (-> ^Date date .toInstant .getEpochSecond)))))
                             to-date (comp (filter (fn [{:keys [date]}]
                                                     (<= (-> ^Date date .toInstant .getEpochSecond)
                                                         (-> ^Date to-date .toInstant .getEpochSecond)))))
                             minimum-free-seats (comp (filter (fn [{:keys [number-of-seats]}]
                                                                (<= minimum-free-seats
                                                                    number-of-seats)))))

                    matched-rides (transduce tf conj rides)]
                (print-matched-rides! matched-rides)
                (recur rides last-ride))
          (do (clojure.pprint/pprint rides)
              (throw (ex-info (str "Invalid command: " command) {:command command}))))))
    (catch ExceptionInfo ex
      (println (ex-message ex))
      (when-let [reason (:reason (ex-data ex))]
        (println reason))
      (print-help-message!))))

(defn -main [& args]
  (prompt!))