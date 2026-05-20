import "../imported-file" ;

# With Comments !
def weird($a; $b; $c):
	[ $a, $b, $c ] | transpose | reduce .[][] as $item (
		[];
		. + $item.property
	)
;

. | weird (.a; .b; .c) |

(

if (. | contains("never") ) then
	"Why yes"
else
	12.23
end

) as $never |

{
	hello,
	why: "because",
	hello: ( weird | ascii_upcase ),
	format_eg: ( . | @json "My json string \( . | this | part | just | white | ascii_upcase | transpose)" ),
	never: $never,
	"literal_key": literal_value,
	"this": 12.1e12,
	"part": "almost"
	"like": [
		12,
		2
		"json"
		{
			"quite": {
				similar: "but not quite"
			}
		}
	],
} | (
	
	# And with very basic brace matching
	
	# Invalid End
	]	
	
	# Other invalid ends
	( [ } ] )

	# A "valid" sequence
	( [  { key: () , other_key:( [ []  [[]] ]  ), gaga }  ] )

	# A "invalid" sequence
	( [  { key: () , other_key:( [ []  [[] ]  ), gaga }  ] )

	"A string\n whith escaped characters \" because we can"
)

