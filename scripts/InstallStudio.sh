#!/bin/bash

user=`whoami`
path=`pwd`

if [ $path != "/usr/local/bin" ] ||  [ $user != "root" ]  ; then
    echo "Ce script doit être placé dans /usr/local/bin et lancé en tant que superutilisateur (root)."
    exit 1
fi

users=$(ls /home/ | tr " " "\n")
invalids=( root lost+found xguest user live )

echo -n "
Script d'installation Studio

Liste des utilisateurs sur cette machine :

"

valids=`comm -23 <(echo ${users[@]} | sed 's/ /\n/g' | sort -u) <(echo ${invalids[@]} | sed 's/ /\n/g' | sort -u)`

usernumber=0
for user in $valids
do
    echo "[$usernumber]> $user"
    usernumber=$(($usernumber+1))
    users[$usernumber]=$user
done

mysize=$((${#users[*]}-1))

echo -e "
####### Sélection de l'utilisateur du studio (entrer le chiffre en regard)
"
read -e -p "####### Choize el user > " myusernumber

index=$((${myusernumber}+1))

if [ "$index" -gt "${mysize}" ] ; then
    echo "invalid user"
    exit 1
fi
    
myuser=${users[${index}]}
echo -e "Utilisateur sélectionné : \033[1m$myuser\033[0m"

echo -e "
Hi "$myuser", On va se faire vite-fait un méchant studio son pour continuer de rocker le monde libre, shall we ?"

echo -e "
####### Ajout de \033[1m$myuser\033[0m au groupe audio"
echo -e "usermod -a -G audio $myuser"
usermod -a -G audio $myuser

if [ -d "/usr/lib/vst" ]; then
    chown .audio  /usr/lib/vst
fi

if [ -d "/usr/local/lib/vst" ]; then
    chmod g+w /usr/local/lib/vst /usr/lib/vst
fi

echo -e "
####### Création des répertoires"

if [ ! -d "/home/$user/Studio" ]; then
    mkdir -p /home/$user/Studio/00-PRESETS
    mkdir -p /home/$user/Studio/00-BANK/SOUND_FONTS
    mkdir /home/$user/Studio/00-SONGS
fi

echo -e "
####### Peuplement des répertoires"
echo "> Fichier de config Qtractor"
wget -nv http://beldigital.net/libs/InstallStudio/Qtractor.conf
sed -i 's/"xix"/"$user"/g' Qtractor.conf
mv -v Qtractor.conf /home/$user/.config/rncbc.org/
echo "> Fichier de session Qtractor"
wget -nv http://beldigital.net/libs/InstallStudio/NEW_SESSION.qtr
mv -v NEW_SESSION.qtr /home/$user/Studio/00-SONGS
echo "> Fichier de connexion QjackCTL"
wget -nv http://beldigital.net/libs/InstallStudio/PatchBay00.xml
mv -v PatchBay00.xml /home/$user/Studio/00-PRESETS

echo "> Icône de lancement du Studio"
if [ -d "/home/$user/Desktop" ]; then
    desktopDir=/home/$user/Desktop
elif [ -d "/home/$user/Bureau" ]; then
    desktopDir=/home/$user/Bureau
fi

echo -e "
[Desktop Entry]
Comment=StartStudio
Exec=StartStudio.sh
GenericName=StartStudio
Icon=tuxguitar
Name=StartStudio
Path=/home/$user/Studio
StartupNotify=true
Terminal=false
Type=Application
X-KDE-SubstituteUID=false
" > $desktopDir/StartStudio.desktop

echo "> Documentation Qtractor"
wget -nv http://downloads.sourceforge.net/qtractor/qtractor-0.3.0-user-manual.pdf
mv -v qtractor-0.3.0-user-manual.pdf $desktopDir/qtractor-user-manual.pdf

chown -R $user.$user /home/$user/Studio $desktopDir/StartStudio.desktop /home/$user/.config/rncbc.org/ $desktopDir/qtractor-user-manual.pdf

echo -e "
####### Reglage du temps réel"
cp -v /etc/security/limits.conf /etc/security/limits.conf.orig 
cp /etc/security/limits.conf ./limits.tmp

sed -i '/^@audio *- *memlock/ d' ./limits.tmp
sed -i '/^@audio *- *rtprio/ d' ./limits.tmp
sed -i '/^@audio *- *nice/ d' ./limits.tmp
sed -i '/^# End of file/ d' ./limits.tmp

echo -e "
@audio          -       memlock          unlimited
@audio          -       rtprio           99
@audio          -       nice             -19

# End of file
" >> ./limits.tmp

cat -s ./limits.tmp > /etc/security/limits.conf
rm ./limits.tmp

echo -e "> Edited /etc/security/limits.conf"
echo -e "@audio          -       memlock          unlimited
@audio          -       rtprio           99
@audio          -       nice             -19"

echo -e "
####### Sélection de la carte son [O/n]"
echo -e ""

cards=`cut -f3 -d\  /proc/asound/cards | cut -c2- | grep -v '^$'`
arr=$(echo $cards | tr " " "\n")

cardnumber=0
for x in $arr
do
    echo "[$cardnumber]> $x"
    cardnumber=$(($cardnumber+1))
        cards[$cardnumber]=$x
    done

    read -e -p "
####### Choize la carte > " mycardnumber
    cardnum=$(($mycardnumber+1))

    MyCard=${cards[${cardnum}]}
    echo -e "
####### Carte sélectionnée : \033[1m$MyCard\033[0m"

echo "
####### > Le serveur de son"
urpmf --uniq --summary qjackctl

echo "
####### > Les sequencers"
urpmf --uniq --summary seq24 qtractor

echo "
####### > Les plugins DSSI"
urpmf --uniq --summary dssi xsynth-dssi fluidsynth-dssi dssi-vst wsynth-dssi dssi-vst-wine whysynth

echo "
####### > Les plugins LADSPA"
urpmf --uniq --summary ladspa calf

echo "
####### > Les plugins LV2"
urpmf --uniq --summary lv2core invada-studio-plugins-lv2 ll-plugins

echo "
####### > Les outils divers"
urpmf --uniq --summary vmpk qmidinet zynjacku tuxguitar flac

echo "
####### > Le mastering"
urpmf --uniq --summary jamin

read -e -p "
####### Ayet, on installe ? [O/n] " yyn
echo -e ""

if [ -z $yyn ] || [ $yyn == "O" ] || [ $yyn == "o" ]; then
    echo "
####### > C'est parti, installation des softs et des dépendances (c'est l'heure du ricard)"

#    urpmi --auto qtractor seq24 qjackctl dssi xsynth-dssi fluidsynth-dssi dssi-vst wsynth-dssi dssi-vst-wine whysynth ladspa calf lv2core invada-studio-plugins-lv2 ll-plugins vmpk qmidinet ams zynjacku tuxguitar flac jamin
   
else
    exit 0
fi

echo -n "
VST_PATH=/usr/lib/vst:/usr/local/lib/vst
export VST_PATH

if [ \"\$1\" == \"-k\" ] ; then
    killall qjackctl qtractor vmpk qmidinet jackd
else
    killall qjackctl qtractor vmpk qmidinet jackd
    sleep 1
    jackd -d alsa -d hw:"$MyCard "&
    sleep 1
    vmpk &
    sleep 1
    qjackctl -a ~/Studio/00-PRESETS/PatchBay00.xml &
    sleep 2
    qmidinet &
    sleep 1

    # Ici Tu peux lancer des synthés, des trucs, en spécifiant un preset ou pas
    # ams -N Guitar -l ~/Studio/00-PRESETS/MyGrt00.ams &
    # sleep 1
    # ams -N BassSynth -l ~/Studio/00-PRESETS/MySynth000.ams &
    
    if [ \"\$1\" == \"-d\" ] ; then
        /usr/local/bin/qtractor \$2 &
    else
        qtractor \$1 &
    fi
fi
" > StartStudio.sh
chown $user.audio StartStudio.sh
chmod u+x StartStudio.sh


echo -e "
Le script de lancement du Studio a été généré, pour le lancer taper 
\033[1mStartStudio.sh\033[0m
en tant que "$user" (et \033[1msurtout\033[0m pas) as root.
Il y a aussi une icône sur le bureau.

NOTES
Les VSTs doivent être placés dans l'un de ces répertoires :
/usr/lib/vst
/usr/local/lib/vst

####### > Un peu de doc :

Tutos 
 Qtractor 
 http://www.slackermedia.info/qtractor/howto_qmidi.html
 Tuto ALSA Modular Synth (un must)
 http://www.linuxformat.com/wiki/index.php/Audio_production_-_Synthesis

Théorie
 Manual QTractor
 http://downloads.sourceforge.net/qtractor/qtractor-0.3.0-user-manual.pdf
 JACK
 http://jackaudio.org/documentation

Videos
 Qtractor & seq24 http://www.youtube.com/watch?v=2E2PWQI-_O0
 Invada plugins http://www.youtube.com/watch?v=86tqYLXYhkE
 JACK http://www.youtube.com/watch?v=fMz6fDGBnA4

Happy Crunching, $user."

read -e -p 

read -e -p "
####### Lancer le studio maintenant ? [O/n] " on
echo -e ""

if [ -z $on ] || [ $on == "O" ] || [ $on == "o" ]; then
    echo "
####### > C'est parti"
    su - $user -c StartStudio.sh
else
    exit 0
fi
