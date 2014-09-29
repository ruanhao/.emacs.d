#! /bin/bash
DIR=$( dirname $0 )
CONFIG_LIST=$( grep -v '^[[:space:]]*#' $DIR/emacs.config )
> $DIR/init.el

for item in $CONFIG_LIST; do
    echo "Generating config ($item)..."
    cat $DIR/features/$item.el >> $DIR/init.el
    echo >> $DIR/init.el
done

echo "Completed"
