<html>
<body>
<?php
    define("NUM_NODOS", 3);
    include './functions.php';

    $text = $wordToSearch = "";

    if ($_SERVER["REQUEST_METHOD"] == "POST") {
        $text = strtolower($_POST["Texto"]);
        $wordToSearch = strtolower($_POST["Word"]);
    }   
    $text = clean($text);
    $keywords = preg_split("/[\s,;.:`'=-]+/", $text);
    $tam_chunks = ceil(count($keywords)/NUM_NODOS);
    $chunks = array_chunk($keywords, $tam_chunks);
    $mapResult = array();

    // each iteration is a map process with filtering
    for($i = 0; $i < NUM_NODOS; $i++)
    {
        $aux = array_map($map, $chunks[$i]);
        $aux2 = array();
        foreach($aux as $a)
        {
            if($a[$wordToSearch])
            {
                $mapResult[] = $a;
            }
        }
    }

    // reduce process
    $countW = 0;
    foreach($mapResult as $a)
    {
        $countW = $countW + $a[$wordToSearch];
    }
    print_r($countW);
?>

</body>
</html>

