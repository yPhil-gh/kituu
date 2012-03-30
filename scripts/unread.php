#!/usr/bin/php
<?php
$host = $argv[1];
$port_and_path = $argv[2];
$imap_dir = $argv[3];
$login = $argv[4];
$pass = $argv[5];
$sep = "|_|";

function mb_imap_to_utf8($str) {
    $convStr = '';
    $subLines = preg_split('/[\r\n]+/',$str); // split multi-line subjects
    for($i=0; $i < count($subLines); $i++){ // go through lines
        $convLine = '';
        $linePartArr = imap_mime_header_decode(trim($subLines[$i])); // split and decode by charset
        for($j=0; $j < count($linePartArr); $j++){
            $convLine .= ($linePartArr[$j]->text); // append sub-parts of line together
        }
        $convStr .= $convLine; // append to whole subject
    }
    return str_replace("|_|", "_", utf8_decode ($convStr)); // return converted subject & protect our sep
}

$inbox = imap_open('{' . $host . ':' . $port_and_path . '}' . $imap_dir , $login, $pass) or die("can't connect: " . imap_last_error());

$emails = imap_search($inbox,'UNSEEN');

/* echo "plop"; */

if ($emails) {
  rsort ($emails);
  foreach ($emails as $email_number) {
    $overview = imap_fetch_overview($inbox,$email_number,0);
    print mb_imap_to_utf8($overview[0]->subject) . $sep;
    print mb_imap_to_utf8(str_replace ('"', '', $overview[0]->from)) . $sep;
    print mb_imap_to_utf8($overview[0]->date) . $sep . "\n";
    /* print mb_imap_to_utf8(imap_fetchbody($inbox,$email_number,1)) . "\n"; */
  }
}
imap_close($inbox);
?>
