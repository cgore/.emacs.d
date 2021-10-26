(cond ((dividend-finance-cgore?)
       (let ((java-home "/Users/chrisgore/Library/Caches/Coursier/arc/https/github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.11%252B9/OpenJDK11U-jdk_x64_mac_hotspot_11.0.11_9.tar.gz/jdk-11.0.11+9/Contents/Home"))
         (setenv "JAVA_HOME" java-home)
         (setq exec-path
               (append (list (concat java-home "/bin"))
                       exec-path))))

      (t (setq exec-path
               (append '(("/usr/local/opt/openjdk/bin"))
                       exec-path))))
