import tofu.logging.logback.ConsoleContextLayout


appender("STDOUT", ConsoleAppender){
    encoder(LayoutWrappingEncoder){
        layout(ConsoleContextLayout){
            pattern = "%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n"
        }
    }
}


root(DEBUG, ["STDOUT"])