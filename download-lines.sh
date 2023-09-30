nmax=$1
osc=$2
elem=$3
sp=$4
wget -r --header="Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"   --header="Accept-Language: en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6"   --header="Cache-Control: max-age=0"   --header="Connection: keep-alive"   --header="Content-Type: application/x-www-form-urlencoded"   --header="DNT: 1"   --header="Origin: https://linelist.pa.uky.edu"   --header="Referer: https://linelist.pa.uky.edu/newpage/"   --header="Sec-Fetch-Dest: document"   --header="Sec-Fetch-Mode: navigate"   --header="Sec-Fetch-Site: same-origin"   --header="Sec-Fetch-User: ?1"   --header="Upgrade-Insecure-Requests: 1"   --header="User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"   --header='sec-ch-ua: "Not/A)Brand";v="99", "Google Chrome";v="115", "Chromium";v="115"'   --header="sec-ch-ua-mobile: ?0"   --header='sec-ch-ua-platform: "Windows"'   --header="sec-gpc: 1"   --compression=auto   --post-data="wavl=1-20000&wave=Angstrom&air=Vacuum&radv=&rvtp=vrad&wacc=&elmion=$elem+$sp&akival=$osc&akitype=f_ik&akizero=incl&abun=&depl=&elo=&ener=eV&ehi=&nmax=$nmax&type=All&auto=Suppress&form=wacc&form=spec&form=type&form=conf&form=term&form=angm&jval=useg&form=prob&tptype=as_a&tptype=as_f&form=ener&mode=HTML+&mlin=5000"      https://linelist.pa.uky.edu/newpage/cgi-bin/qlines.cgi
cd linelist.pa.uky.edu/
cd newpage/
cd cgi-bin/
grep -h "$elem $sp" qmult* | sort > $elem$sp.txt
cat $elem$sp.txt| colrm 20 29  |sed "s/  $sp/$elem $sp/g" >/mnt/c/work4/plasma/$elem$sp.txt
