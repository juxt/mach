;; Copyright © 2016-2017, JUXT LTD.

(ns aero.aws
  (:require
   [cljs.nodejs :as nodejs]
   [aero.core :refer [reader]]))

;; AWS credentials parsing

(def fs (nodejs/require "fs"))
(def ini (nodejs/require "ini"))
(def path (nodejs/require "path"))

(defmethod reader 'aws/credentials
  [opts tag value]
  (let [creds (get (js->clj (ini.parse (fs.readFileSync (path.join js/process.env.HOME "/.aws/credentials") "utf-8"))) value)]
    {:aws-access-key-id (get creds "aws_access_key_id")
     :aws-secret-access-key (get creds "aws_secret_access_key")}))
