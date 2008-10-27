<?php

/* vim: set expandtab tabstop=4 softtabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2002 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.0 of the PHP license,       |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Evgeny Stepanischev <se@timelabs.ru>                        |
// +----------------------------------------------------------------------+
// Project home page (Russian): http://bolk.exler.ru/files/figlet/
//
// $Id$

class Text_Figlet
{
    /**
     * Height of a letter
     *
     * @var integer
     *
     * @access private
     */

    var $height;

    /**
     * Letter baseline
     *
     * @var integer
     *
     * @access private
     */

    var $oldlayout;

    /**
     * Flag - RTL (right to left) or LTR (left to right) text direction
     *
     * @var integer
     *
     * @access private
     */

    var $rtol;

    /**
     * Information about special 'hardblank' character
     *
     * @var integer
     *
     * @access private
     */

    var $hardblank;

    /**
     * Is used for keeping font
     *
     * @var array
     *
     * @access private
     */

    var $Font;

    /**
     * Flag is true if smushing occured in letters printing cycle
     *
     * @var integer
     *
     * @access private
     */

    var $smush_flag;

    /**
     * Text_Figlet constructor. Does nothing right now, just calls
     * parent constuctor.
     *
     * @access public
     */

    function Text_Figlet()
    {
        return true;
    }

    /**
     * Load user font. Must be invoked first.
     *
     * @param string $filename font file name
     * @param bool $loadgerman (optional) load German character set or not
     * @access public
     * @return mixed PEAR_error or true for success
     */

    function LoadFont($filename, $loadgerman = true)
    {
        $this->Font = array();
        if (!file_exists($filename))
        return $this->raiseError('File is not found', 1);

        // If Gzip compressed font
        if (substr($filename, -3, 3) == '.gz') {
            $filename = "compress.zlib://$filename";
            $compressed = true;

            if (!function_exists('gzcompress'))
            return $this->raiseError('Unknown FIGlet font format.', 3);
        } else {
            $compressed = false;
        }

        if ($fp = fopen($filename, 'rb')) {
            if (!$compressed) flock($fp, LOCK_SH);

//            flf2a$ 6 5 20 15 3 0 143 229
//              |  | | | |  |  | |  |   |
//             /  /  | | |  |  | |  |   \
//    Signature  /  /  | |  |  | |   \   Codetag_Count
//      Hardblank  /  /  |  |  |  \   Full_Layout
//           Height  /   |  |   \  Print_Direction
//           Baseline   /    \   Comment_Lines
//            Max_Length      Old_Layout


            $header = explode(' ', fgets($fp, 2048));

            if (substr($header[0], 0, 5) <> 'flf2a')
            return $this->raiseError('Unknown FIGlet font format.', 3);

            @list ($this->hardblank, $this->height,,,
            $this->oldlayout, $cmt_count, $this->rtol) = $header;

            $this->hardblank = substr($this->hardblank, -1, 1);

            for ($i = 0; $i<$cmt_count; $i++)
            fgets($fp, 2048);

            // ASCII charcters
            for ($i = 32; $i<127; $i++)
            $this->Font[$i] = $this->_char($fp);

            foreach (array(91, 92, 93, 123, 124, 125, 126) as $i)
            if ($loadgerman) {
                $letter = $this->_char($fp);

                // Invalid character but main font is loaded and I can use it
                if ($letter === false) {
                    fclose($fp);
                    return true;
                }

                // Load if it is not blank only
                if (trim(implode('', $letter)) <> '')
                $this->Font[$i] = $letter;
            } else {
                $this->_skip($fp);
            }

            // Extented characters
            for ($n = 0; !feof($fp); $n++) {
                list ($i) = explode(' ', rtrim(fgets($fp, 1024)), 2);
                if ($i == '') continue;

                // If comment
                if (preg_match('/^\-0x/i', $i)) {
                    $this->_skip($fp);
                } else {
                    // If Unicode
                    if (preg_match('/^0x/i', $i))
                    $i = hexdec(substr($i, 2));
                    else
                    // If octal
                    if ($i{0} === '0' && $i !== '0' || substr($i, 0, 2) == '-0')
                    $i = octdec($i);

                    $letter = $this->_char($fp);

                    // Invalid character but main font is loaded and I can use it
                    if ($letter === FALSE) {
                        fclose($fp);
                        return true;
                    }

                    $this->Font[$i] = $letter;
                }
            }

            fclose($fp);
            return true;
        } else {
            return $this->raiseError('Cannot open font file', 2);
        }
    }

    /**
    * Print string using font loaded by LoadFont method
    *
    * @param string $str string for printing
    * @param bool $inhtml (optional) output mode - HTML (true) or plain text (false)
    * @access public
    * @return string contains 
    */

    function LineEcho($str, $inhtml = false)
    {
        $out = array();

        for ($i = 0; $i<strlen($str); $i++) {
            // Pseudo Unicode support
            if (substr($str, $i, 2) == '%u') {
                $lt = hexdec(substr($str, $i+2, 4));
                $i += 6;
            }
            else
            $lt = ord($str{$i});

            $hb = preg_quote($this->hardblank, '/');
            $sp = "$hb\\x00\\s";

            // If chosen character not found try to use default
            // If default character is not defined skip it

            if (!isset($this->Font[$lt]))
            if (isset($this->Font[0])) $lt = 0; else continue;

            for ($j = 0; $j<$this->height; $j++) {
                $line = $this->Font[$lt][$j];

                // Replace hardblanks
                if (isset($out[$j])) {
                    if ($this->rtol)
                    $out[$j] = $line . $out[$j]; else
                    $out[$j].= $line;
                } else {
                    $out[$j] = $line;
                }
            }

            if ($this->oldlayout > -1 && $i) {
                // Calculate minimal distance between two last letters

                $mindiff = -1;

                for ($j = 0; $j<$this->height; $j++)
                if (preg_match("/\S(\s*\\x00\s*)\S/", $out[$j], $r))
                $mindiff = $mindiff == -1 ? strlen($r[1]) : min($mindiff, strlen($r[1]));

                // Remove spaces between two last letter
                // dec mindiff for exclude \x00 symbol

                if (--$mindiff > 0)
                for ($j = 0; $j<$this->height; $j++)
                if (preg_match("/\\x00(\s{0,{$mindiff}})/", $out[$j], $r)) {
                    $b = $mindiff - ($l = strlen($r[1]));
                    $out[$j] = preg_replace("/\s{0,$b}\\x00\s{{$l}}/", "\0", $out[$j], 1);

                }
                // Smushing

                $this->smush_flag = 0;

                for ($j = 0; $j<$this->height; $j++)
                $out[$j] = 
                preg_replace_callback
                ("#([^$sp])\\x00([^$sp])#", array(&$this, '_rep'), $out[$j]);

                // Remove one space if smushing
                // and remove all \x00 except tail whenever

                if ($this->smush_flag) {
                    $pat = array("/\s\\x00(?!$)|\\x00\s/", "/\\x00(?!$)/");
                    $rep = array('', '');
                } else {
                    $pat = "/\\x00(?!$)/";
                    $rep = '';
                }

                for ($j = 0; $j<$this->height; $j++)
                $out[$j] = preg_replace($pat, $rep, $out[$j]);
            }
        }

        $trans = array("\0" => '', $this->hardblank => ' ');
        $str = strtr(implode("\n", $out), $trans);

        if ($inhtml)
        return '<nobr>'.nl2br(str_replace(' ', '&nbsp;', htmlspecialchars($str))).'</nobr>';

        return $str;
    }

    /**
    * It is preg_replace callback function that makes horizontal letter smushing
    *
    * @param array $r
    * @return string
    * @access private
    */

    function _rep($r)
    {
        if ($this->oldlayout & 1 && $r[1] == $r[2]) {
            $this->smush_flag = 1;
            return $r[1];
        }

        if ($this->oldlayout & 2) {
            $symb = '|/\\[]{}()<>';

            if ($r[1] == '_' && strpos($symb, $r[2]) !== FALSE ||
                $r[2] == '_' && strpos($symb, $r[1]) !== FALSE) {
                $this->smush_flag = 1;
                return $r[1];
            }
        }

        if ($this->oldlayout & 4) {
            $classes = '|/\\[]{}()<>';

            if (($left = strpos($classes, $r[1])) !== FALSE) {
                if (($right = strpos($classes, $r[2])) !== FALSE) {
                    $this->smush_flag = 1;
                    return $right > $left ? $r[2] : $r[1];
                }
            }
        }

        if ($this->oldlayout & 8) {
            $t = array('[' => ']', ']' => '[', '{' => '}', '}' => '{',
            '(' => ')', ')' => '(');

            if (isset($t[$r[2]]) && $r[1] == $t[$r[2]]) {
                $this->smush_flag = 1;
                return '|';
            }
        }

        if ($this->oldlayout & 16) {
            $t = array("/\\" => '|', "\\/" => 'Y', '><' => 'X');

            if (isset($t[$r[1].$r[2]])) {
                $this->smush_flag = 1;
                return $t[$r[1].$r[2]];
            }
        }

        if ($this->oldlayout & 32) {
            if ($r[1] == $r[2] && $r[1] == $this->hardblank) {
                $this->smush_flag = 1;
                return $this->hardblank;
            }
        }

        return $r[1]."\00".$r[2];
    }


    /**
    * Function loads one character in the internal array from file
    *
    * @param resource $fp handle of font file
    * @return mixed lines of the character or false if foef occured
    * @access private
    */

    function _char(&$fp)
    {
        $out = array();

        for ($i = 0; $i < $this->height; $i++) {
            if (feof($fp)) return false;

            $line = rtrim(fgets($fp, 2048), "\r\n");
            if (preg_match('/(.){1,2}$/', $line, $r))
            $line = str_replace($r[1], '', $line);

            $line .= "\x00";

            $out[] = $line;
        }
                                                                            
        return $out;
    }

    /**
    * Function for skipping one character in a font file
    *
    * @param resource $fp handle of font file
    * @return bool always return true
    * @access private
    */

    function _skip(&$fp)
    {
        for ($i = 0; $i<$this->height && !feof($fp); $i++)
        fgets($fp, 2048);

        return true;
    }

    /**
    * Simple returns error code and error message
    *
    * @param string $message
    * @param integer $code
    * @return array
    * @access private
    */

    function raiseError($message, $code)
    {
        return array('code' => $code, 'message' => $message);
    }
}
?>