<?php

include(dirname(__FILE__).'/config.php');

// recreating database
// CAUTION!!!!!!!! the below line kills entire database, uncomment and use with care
# exec("curl -X DELETE $HOSTDB");
exec("curl -X PUT $HOSTDB");

$list = `find ../examples -type f`;
//$list = `find ../convert-examples/examples-converted -type f -name '*.mm'`;
$files = split("\n", $list);
foreach ($files as $file) {
	if ($file) {
		$com = "php push-filter.php '$file'";
		// echo "$com\n";
		echo "$file\n";
		passthru($com);
	}
		
}

