import tofu.logging.logback.ConsoleContextLayout
import tofu.logging.ELKLayout


appender("STDOUT", ConsoleAppender){
    encoder(LayoutWrappingEncoder){
        layout(ELKLayout){
            pattern = "%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n"
        }
    }
}

appender("STDOUT2", ConsoleAppender){
    encoder(LayoutWrappingEncoder){
        layout(ConsoleContextLayout){
            pattern = "%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n"
        }
    }
}


root(INFO, ["STDOUT", "STDOUT2"])