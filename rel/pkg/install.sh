#!/usr/bin/bash

[ -f /usbkey/config ] && . /usbkey/config

TESTED_VERSIONS=20151001T070028Z\|20151104T185720Z\|20160204T080230Z\|20160204T173339Z
BAD_VERSIONS=2012\|2013\|2014\|20150528T153328Z


## 20150528T153328Z - kstat bug, keeps growing indefinetly

if [ -z "$DST" ]
then
    DST="/opt"
fi

function graylist {
    ver="$1"
    msg="$2"
    if uname -a | egrep $ver
    then
        echo $msg
        echo
        echo "This SmartOS release is affected by the abovementioned problem!"
        echo "Would you like to continue non the less? [yes|NO] "
        read SKIP
        if [[ "$SKIP" = "yes" ]]
        then
            echo "Okay we go on, but it might not work!"
        else
            echo "Exiting."
            exit 1
        fi
    fi
}

#IFACE=`dladm show-phys -m | grep $admin_nic | awk '{print $1}'`
#IP=`ifconfig $IFACE | grep inet | awk '{print $2}'`

DIR=$(dirname "$0");
if [[ "$DIR" = "." ]]
then
    DIR=$(pwd)
fi
BASE=$(basename "$0");

FORCE=false
while getopts ":f" opt; do
    case $opt in
        f)
            FORCE=true
            ;;
    esac
done

if uname -a | egrep $BAD_VERSIONS
then
    echo "Sorry this SmartOS version is known to be incompatible or faulty."
    exit 1
fi

if [ "$FORCE" = false ] ; then
    if uname -a | egrep $TESTED_VERSIONS
    then
        echo "This SmartOS release is tested!"
    else
        echo "This SmartOS release WAS NOT tested! Are you sure you want to go on? [yes|NO] "
        read SKIP
        if [[ "$SKIP" = "yes" ]]
        then
            echo "Okay we go on, but it might not work!"
        else
            echo "Exiting."
            exit 1
        fi
    fi
fi

# We've to initialize imgadm or it will die horribly .... *sigh*
imgadm update
[ -d /var/imgadm/images ] || mkdir -p /var/imgadm/images



[ -d /opt/chunter/share ] && rm -r /opt/chunter/share

(cd "$DST"; uudecode -p "$DIR/$BASE"| tar xf -)
mkdir -p /var/log/chunter


## Generate all the needed values
conf_admin_mac=$(echo "$admin_nic" | sed 's/0\([0-9]\)/0?\1/g')
case "$conf_admin_mac" in
    aggr*)
        conf_admin_nic="$conf_admin_mac"
        ;;
    *)
        conf_admin_nic=$(dladm show-phys -m -o LINK,ADDRESS | /usr/bin/egrep "$conf_admin_mac" | awk '{print $1}')
        ;;
esac
conf_admin_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_admin_nic" | awk '{print $2}' | awk -F/ '{print $1}')

conf_fifo_nic=fifo0
if ipadm show-addr -o ADDROBJ | grep "^$conf_fifo_nic" > /dev/null
then
    conf_fifo_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_fifo_nic" | awk '{print $2}' | awk -F/ '{print $1}')
    conf_admin_ip=$conf_fifo_ip
fi

CONFFILE="${DST}/chunter/etc/chunter.conf"
if [ ! -f $CONFFILE ]
then
    echo "Creating new configuration from example file."
    if [[ "$conf_admin_ip" = "" ]]
    then
        cp ${CONFFILE}.example ${CONFFILE}
    else
        sed "s/^ip = 127.0.0.1:4200/ip=$conf_admin_ip:4200/" ${CONFFILE}.example > ${CONFFILE}
    fi
else
    echo "Please make sure you update your config according to the update manual!"
    #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
    #    mv ${CONFFILE} ${CONFFILE}.old &&
    #    mv ${CONFFILE}.new ${CONFFILE}

fi

mkdir -p "$DST/custom/smf"
cp "$DST/chunter/share/chunter.xml" "$DST/custom/smf"

svccfg import "$DST/custom/smf/chunter.xml"

exit 0;
