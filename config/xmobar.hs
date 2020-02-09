Config {
       font = "xft::pixelsize=13",
       commands = [
                -- Addison, TX
                Run CoreTemp ["-t", "Temp: <core0>°c"] 10,
                Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10,
                Run Memory ["-t","Mem: <used>/<total>M(<usedratio>%)"] 10,
                Run Swap [] 10,
                Run Date "%a %b %_d %H:%M" "date" 10,
                Run Network "eno1" [] 10,
                Run StdinReader
                ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% / %memory% / %coretemp% / %eno1%    <fc=#ee9a00>%date%</fc>"
       }
