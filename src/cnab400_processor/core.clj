(ns cnab400-processor.core
  (:require [clojure.java.io :as io]
            [cnab400-processor.cnab :as c]
            [clojure.string :as s]
            [cheshire.core :as j]
            [clj-time.format :as f]
            [dk.ative.docjure.spreadsheet :as sp])
  (:gen-class))

(def custom-formatter (f/formatter "yyyy-MM-dd"))

(extend-protocol cheshire.generate/JSONable
  org.joda.time.DateTime
  (to-json [dt gen]
    (cheshire.generate/write-string gen (f/unparse custom-formatter dt))))

(defn to-json
  "converts a cnab-400 file to json and save with the given file name"
  [filename blob]
  (let [newfile   (str filename ".json")
        json-blob (j/generate-string (c/cnab400 blob) {:pretty true})]

    (spit newfile json-blob)
    (println "json created -> " newfile)))

(defn process-ret
  "process a ret file, given path and filename"
  [path filename]
  (let [fullpath (if (s/ends-with? path "/")
                   (str path filename)
                   (str path "/" filename))]

    (println "processing ret" filename)
    (to-json fullpath (slurp fullpath))))

(defn process-ret-files
  "converts to json each .cnab file inside a given folder path"
  [path]
  (if (.isDirectory (io/file path))
    (let [files (map str (.list (io/file path)))
          cnabs (filter #(s/ends-with? % ".RET") files)]

      (doseq [cnab cnabs]
        (process-ret path cnab)))

    (println "ERROR:" path "isn't a folder path")))

(defn init-xlsx [file-path]
  (let [wb (sp/create-workbook "retorno"
                               [["account" "agency" "bank-agency" "bank-agency-digit" "bank-code" "billing-fare"
                                 "cancelled-instruction" "company-use" "credit-date" "digit" "discount"	"document-number"
                                 "due-date"	"errors" "inscription-code"	"inscription-number" "interest"	"invoice-digit"
                                 "iof" "liquidation-code" "main-value" "name" "occurrence-code"	"occurrence-date"
                                 "other-credits" "our-number" "our-number-2" "our-number-3" "our-number-digit"
                                 "reduction-value" "register-type" "sequential-number" "species" "value" "wallet" "whites"
                                 "whites-2"	"whites-3" "whites-4" "whites-5" "whites-6"	"whites-7" "whites-8" "whites-9" "zeros" "zeros-2"]])]

    (sp/save-workbook! file-path wb)

    wb))

(defn build-row
  [reg headers]
  (map #(get reg %) headers))

(defn reg-to-xlsx
  [reg sheet headers]
  (sp/add-row! sheet (build-row reg headers)))

(defn to-xlsx
  "converts a json file to xlsx rows and append the data to xlsx-file parameter"
  [input-data sheet header-row]
  (let [entries (get input-data "entries")]

     (doseq [entry entries]
       (reg-to-xlsx entry sheet header-row))))

(defn from-json
 "read .json file and returns map data"
 [input-file-path]

 (j/parse-stream (clojure.java.io/reader input-file-path)))

(defn process-json
  "process a json file, given path and filename"
  [path filename xlsx-file-path wb]
  (let [fullpath    (if (s/ends-with? path "/") (str path filename) (str path "/" filename))
        _           (println "processing json " filename)
        json-data   (from-json fullpath)
        sheet       (sp/select-sheet "retorno" wb)
        header-row  (map sp/read-cell (first (sp/row-seq sheet)))]

    (to-xlsx (get json-data "cnab400") sheet header-row)

    (sp/save-workbook! xlsx-file-path wb)))

(defn process-json-files
  "reads a .json file with cnab data and appends it to xlsx file"
  [json-folder-path xlsx-file-path]
  (if (.isDirectory (io/file json-folder-path))
    (let [files (map str (.list (io/file json-folder-path)))
          jsons (sort (filter #(s/ends-with? % ".json") files))
          wb    (init-xlsx xlsx-file-path)]

      (doseq [json jsons]
        (process-json json-folder-path json xlsx-file-path wb)))

    (println "ERROR:" json-folder-path "isn't a folder path")))

(defn -main
  "runnable"
  [& args]
  (let [param           (first args)
        path            (if (nil? param) "cnab" param)
        output-filepath (if (s/ends-with? path "/") (str path "output.xlsx") (str path "/" "output.xlsx"))]
    (println "starting .RET->.JSON looking for .ret files at " path)
    (process-ret-files path)
    (process-json-files path output-filepath)
    (println "done")))
