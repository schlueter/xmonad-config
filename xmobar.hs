--  Author: Brandon Schlueter

Config { position = Top
       , allDesktops = True
       , template = "%StdinReader% }{ %kbd%  %battery%   |   %date%"
       , commands = [
           Run StdinReader
         , Run Kbd [("us(dvorak)", "DV"), ("us", "US")]
         , Run BatteryP ["BAT0"] ["-t", "<left>% <acstatus>", "-H", "80", "-p", "3", "--", "-i", "(On)","-O", "(On)", "-o", "(<timeleft>)", "-l", "red", "-m", "blue", "-h", "green"] 60
         , Run Date "%F %a %T" "date" 10
       ]
}
