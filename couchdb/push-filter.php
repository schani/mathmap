<?php

include(dirname(__FILE__).'/config.php');

$file = $argv[1];

if (! $file) {
	die("Uasge: push-filter.php <filter file>\n");
}

$id = null;
$title = null;
$tags = array();
$kind = 'expression';

$content = '';
$f = fopen($file, "rb");
$last_line = '';
while ($line = fgets($f)) {
	$last_line = $line;
	$content .= $line;
	if (preg_match('!#.*@([a-z]+)\s+(\S.*)!', $line, $m)) {
		$tag = $m[1];
		$value = trim($m[2]);
		switch ($tag) {
			case 'title':
				$title = $value;
				break;
			case 'tags':
				$arr = split(',', $value);
				foreach ($arr as $el)
					$tags[] = trim($el);
				break;
		}
	} else if (preg_match('!filter\s+(\S+)\s*\(!', $line, $m)) {
		// if ($id)
		//	die("Alert! Multiple filter names found in file: $file\n");
		// last filter is main filter
		$id = $m[1];
	}
}

// retrieving id from mmc (from last line)
if (! $id) {
	if (preg_match('!:name\s+"(\S+)"!', $last_line, $m)) {
		$id = "org.mathmap.".$m[1];
		$kind = 'design';
	}
}
if (! $tags) {
	if (preg_match('!:tags\s+\("(\S+)"\)!', $last_line, $m)) {
		$tags = array($m[1]);
	}
}

if (! $id)
	die("Alert! No filter name found in file: $file\n");

$doc = array();

$doc['_id'] = $id;

if ($title)
	$doc['title'] = $title;

if (sizeof($tags))
	$doc['tags'] = $tags;
else
	echo "Warning: no tags found in $file\n";

$doc['content'] = $content;

$doc['kind'] = $kind; // expression type "expression"

$doc_json = json_encode($doc);

$doc_json_esc = str_replace("'", "'\"'\"'", $doc_json);

$com = 'curl -X POST '.$HOSTDB.' -H \'Content-Type: application/json\' -d \''.$doc_json_esc."'";

#echo "$com\n";
system($com);

?>
