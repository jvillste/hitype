(defproject hitype "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [flow-gl/flow-gl "3"]
                 [medley "1.3.0"]
                 [clojure.java-time "0.3.3"]
                 [time-literals "0.1.4"]
                 [overtone "0.16.3331"]
                 [overtone/midi-clj "0.5.0"]]
  :repl-options {:init-ns hitype.core}
  :jvm-opts ^:replace [])
