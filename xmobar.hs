--  Author: Brandon Schlueter

Config { font = "-*-terminal-medium-*-*-*-14-*-*-*-*-*-*-*"
       , position = Top
       , allDesktops = True
       , template = "%StdinReader% }{ %wi% %kbd%  %battery%   %date%"
       , commands = [
           Run StdinReader
         , Run Kbd [("us(dvorak)", "DV"), ("us", "US")]
         , Run BatteryP ["BAT0"] ["-t", "<left>% <acstatus>", "-H", "80", "-p", "3", "--", "-i", "(On)","-O", "(On)", "-o", "(<timeleft>)", "-l", "red", "-m", "blue", "-h", "green"] 60
         , Run Date "%F %a %T" "date" 10
         , Run Wireless "wlp3s0" ["-t", "<essid> <quality"] 60
       ]
}
