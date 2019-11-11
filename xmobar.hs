--  Author: Brandon Schlueter

Config { font = "-*-terminal-medium-*-*-*-18-*-*-*-*-*-*-*"
       , position = Bottom
       , allDesktops = True
       , template = "%StdinReader% }{ %kbd%  %battery%   %date%"
       , commands = [
           Run StdinReader
         , Run Kbd [("us(dvorak)", "DV"), ("us", "US")]
         , Run BatteryP ["BAT0"] ["-t", "<left>% <acstatus>", "-H", "80", "-p", "3", "--", "-i", "(On)","-O", "(On)", "-o", "(<timeleft>)", "-l", "red", "-m", "blue", "-h", "green"] 100
         , Run Date "%F %a %H:%M" "date" 100
       ]
}
