(defproject falx "0.1.0-SNAPSHOT"
            :description "Tactical Dungeoneering"
            :url "http://github.com/danstone/falx"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                           [org.clojure/core.memoize "0.5.6"]
                           [org.clojure/core.logic "0.8.8"]
                           [org.clojure/tools.logging "0.3.1"]
                           [org.clojure/data.int-map "0.1.0"]
                           [log4j "1.2.16"]
                           [gdx-loopy "0.1.0-SNAPSHOT"]
                           [gdx-2d "0.1.0-SNAPSHOT"]
                           [clj-tiny-astar "0.1.1-SNAPSHOT"]
                           [silc "0.1.6"]
                           [cheshire "5.3.1"]
                           [clj-tuple "0.1.7"]]
            :main falx.core
            :repl-options {:init-ns falx.core
                           :init (-main)}
            :global-vars {*warn-on-reflection* true}
            :java-source-paths ["src-java"]
            :jvm-opts ["-server" "-Xmx1g" "-Xms1g"]
            :repositories [["sonatype"
                            "https://oss.sonatype.org/content/repositories/releases/"]]
            :profiles {:dev {:dependencies   [[org.clojure/tools.trace "0.7.8"]
                                              [org.clojure/test.check "0.6.1"]]
                             :resource-paths ["test-resources"]}})
