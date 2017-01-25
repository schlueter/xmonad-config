--  Author: Brandon Schlueter

-- Based on:
--  xmobar config used by Vic Fryzel
--  Author: Vic Fryzel
--  http://github.com/vicfryzel/xmonad-config

Config { font = "xft:Fixed-8"
       , additionalFonts = []
       , position = Top
       , lowerOnStart = True
       , pickBroadest = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , commands = [
           Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10
         , Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
         , Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
         , Run Network "wlp3s0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
         , Run Date "%a %b %_d %l:%M" "date" 10
         , Run StdinReader
         , Run Kbd [("us(dvorak)", "DV"), ("us", "US")]
         , Run BatteryP ["BAT0"] ["-t", "<left>% <acstatus>", "-H", "80", "-p", "3", "--", "-i", "(On)","-O", "(On)", "-o", "(<timeleft>)", "-l", "red", "-m", "blue", "-h", "green"] 60
       ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %kbd%  %battery%  %multicpu%  %memory%  %swap%  %wlp3s0%   <fc=#FFFFCC>%date%</fc>"
       , overrideRedirect = False
}
