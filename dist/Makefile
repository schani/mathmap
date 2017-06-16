all : language.html reference.html

language.html : language.chtml mathmap.chh
	chpp language.chtml >language.html

reference.html : reference.chtml ../builtins_doc.html
	chpp reference.chtml >reference.html
