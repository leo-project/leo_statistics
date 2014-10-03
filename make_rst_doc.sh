#!/bin/sh

make doc
rm -rf doc/rst && mkdir doc/rst

for Mod in leo_metrics_req \
           leo_metrics_req_notifier \
           leo_metrics_vm \
           leo_metrics_vm_notifier \
           leo_statistics_api \
           leo_statistics_sampler \
           leo_statistics_snmpm \
           leo_statistics_user
do
    read_file="doc/$Mod.html"
    write_file="doc/rst/$Mod.rst"

    pandoc --read=html --write=rst "$read_file" -o "$write_file"

    sed -ie "1,6d" "$write_file"
    sed -ie "1s/\Module //" "$write_file"
    LINE_1=`cat $write_file | wc -l`
    LINE_2=`expr $LINE_1 - 10`
    sed -ie "$LINE_2,\$d" "$write_file"
done
rm -rf doc/rst/*.rste
