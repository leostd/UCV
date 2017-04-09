<?php
    $map = function ($str)
    {
        return array($str => 1);
    };

    function clean($string) {
        $string = str_replace(' ', '-', $string); // Replaces all spaces with hyphens.
        return preg_replace('/[^A-Za-z0-9áéíóú\-]/', ' ', $string); // Removes special chars.
    }

?>