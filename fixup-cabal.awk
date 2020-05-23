# https://www.grymoire.com/Unix/Awk.html
# for some reason hpack generates a double module for Cut.SphinxBindings,
# so we just remove it
BEGIN { x=0; }
/Cut.SphinxBindings/ { x++; x%=2; if (x % 2 == 0) { print "      " $1 }} 
!/Cut.SphinxBindings/ { print }
END { }
