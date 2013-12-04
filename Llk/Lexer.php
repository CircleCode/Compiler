<?php

/**
 * Hoa
 *
 *
 * @license
 *
 * New BSD License
 *
 * Copyright © 2007-2013, Ivan Enderlin. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Hoa nor the names of its contributors may be
 *       used to endorse or promote products derived from this software without
 *       specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

namespace {

from('Hoa')

/**
 * \Hoa\Compiler\Exception\UnrecognizedToken
 */
-> import('Compiler.Exception.UnrecognizedToken')

/**
 * \Hoa\Compiler\Exception\Lexer
 */
-> import('Compiler.Exception.Lexer');

}

namespace Hoa\Compiler\Llk {

/**
 * Class \Hoa\Compiler\Llk\Lexer.
 *
 * PP lexer.
 *
 * @author     Frédéric Dadeau <frederic.dadeau@femto-st.fr>
 * @author     Ivan Enderlin <ivan.enderlin@hoa-project.net>
 * @copyright  Copyright © 2007-2013 Frédéric Dadeau, Ivan Enderlin.
 * @license    New BSD License
 */

class Lexer {

    const LEADING_SPACES = 'LEADING_SPACES';
    const SAMEINDENT = 'SAMEINDENT';
    const INDENT = 'INDENT';
    const DEDENT = 'DEDENT';

    /**
     * Lexer state.
     *
     * @var \Hoa\Compiler\Llk\Lexer array
     */
    protected $_lexerState  = null;

    /**
     * Text.
     *
     * @var \Hoa\Compiler\Llk\Lexer string
     */
    protected $_text        = null;

    /**
     * Tokens.
     *
     * @var \Hoa\Compiler\Llk\Lexer array
     */
    protected $_tokens      = array();

    /**
     * Namespace stacks.
     *
     * @var \SplStack object
     */
    protected $_nsStack     = null;

    /**
     * indentation levels stack
     *
     * @var array
     */
    protected $_indentStack = Array(0);

    protected $_options = null;

    public function __construct( Array $options = array() ) {
        $this->_options = array(
            'offSideRules:parse' => true,
            'offSideRules:keepSameIndent' => true,
            'offSideRules:tabstop' => 8
        );
        $this->_options = array_merge($this->_options, $options);
    }


    /**
     * Text tokenizer: splits the text in parameter in an ordered array of
     * tokens.
     *
     * @access  protected
     * @param   string  $text      Text to tokenize.
     * @param   array   $tokens    Tokens to be returned.
     * @return  array
     * @throw   \Hoa\Compiler\Exception\UnrecognizedToken
     */
    public function lexMe ( $text, Array $tokens ) {

        $this->_text       = $text;
        $this->_tokens     = $tokens;
        $this->_nsStack    = null;
        $offset            = 0;
        $maxOffset         = strlen($this->_text);
        $tokenized         = array();
        $this->_lexerState = 'default';
        $stack             = false;

        foreach($this->_tokens as &$tokens) {

            $_tokens = array();
            if($this->_options['offSideRules:parse'])
                $_tokens[self::LEADING_SPACES] = Array('\R\s*', null);

            foreach($tokens as $fullLexeme => $regex) {

                if(false === strpos($fullLexeme, ':')) {

                    $_tokens[$fullLexeme] = array($regex, null);
                    continue;
                }

                list($lexeme, $namespace) = explode(':', $fullLexeme, 2);

                $stack |= ('__shift__' === substr($namespace, 0, 9));

                unset($tokens[$fullLexeme]);
                $_tokens[$lexeme] = array($regex, $namespace);
            }

            $tokens = $_tokens;
        }

        if(true == $stack)
            $this->_nsStack = new \SplStack();

        while($offset < $maxOffset) {

            $nextToken = $this->nextToken($offset);

            if(null === $nextToken)
                throw new \Hoa\Compiler\Exception\UnrecognizedToken(
                    'Unrecognized token "%s" at line 1 and column %d:' .
                    "\n" . '%s' . "\n" .
                    str_repeat(' ', mb_strlen(substr($text, 0, $offset))) . '↑',
                    0, array(
                        mb_substr(substr($text, $offset), 0, 1),
                        $offset + 1,
                        $text
                    ), 1, $offset);

            if($this->_options['offSideRules:parse'] && self::LEADING_SPACES === $nextToken['token']){
                $nextToken['keep'] = false;
                foreach($this->generateOffSideTokens($nextToken, $offset) as $offsideToken){
                    $tokenized[] = $offsideToken;
                }
            }

            if(true === $nextToken['keep']) {
                $nextToken['offset'] = $offset;
                $tokenized[]         = $nextToken;
            }

            $offset += strlen($nextToken['value']);
        }

        $tokenized[] = array(
            'token'     => 'EOF',
            'value'     => 'EOF',
            'length'    => 0,
            'namespace' => 'default',
            'keep'      => true,
            'offset'    => $offset
        );

        return $tokenized;
    }

    /**
     * Compute the next token recognized at the beginning of the string.
     *
     * @access  protected
     * @param   int  $offset    Offset.
     * @return  array
     * @throw   \Hoa\Compiler\Exception\UnrecognizedToken
     */
    protected function nextToken ( $offset ) {

        $tokenArray = &$this->_tokens[$this->_lexerState];

        foreach($tokenArray as $lexeme => $bucket) {

            list($regex, $nextState) = $bucket;

            if(null === $nextState)
                $nextState = $this->_lexerState;

            $out = $this->matchLexeme($lexeme, $regex, $offset);

            if(null !== $out) {

                $out['namespace'] = $this->_lexerState;
                $out['keep']      = 'skip' !== $lexeme;

                if($nextState !== $this->_lexerState) {

                    $shift = false;

                    if(   null !== $this->_nsStack
                       &&    0 !== preg_match('#^__shift__(?:\s*\*\s*(\d+))?$#', $nextState, $matches)) {

                        $i = isset($matches[1]) ? intval($matches[1]) : 1;

                        if($i > ($c = count($this->_nsStack)))
                            throw new \Hoa\Compiler\Exception\Lexer(
                                'Cannot shift namespace %d-times, from token ' .
                                '%s in namespace %s,  because the stack ' .
                                'contains only %d namespaces.',
                                1, array($i, $lexeme, $this->_lexerState, $c));

                        while(1 <=  $i--)
                            $previousNamespace = $this->_nsStack->pop();

                        $nextState = $previousNamespace;
                        $shift     = true;
                    }

                    if(!isset($this->_tokens[$nextState]))
                        throw new \Hoa\Compiler\Exception\Lexer(
                            'Namespace %s does not exist, called by token %s ' .
                            'in namespace %s.',
                            2, array($nextState, $lexeme, $this->_lexerState));

                    if(null !== $this->_nsStack && false === $shift)
                        $this->_nsStack[] = $this->_lexerState;

                    $this->_lexerState = $nextState;
                }

                return $out;
            }
        }

        return null;
    }

    /**
     * Check if a given lexeme is matched at the beginning of the text.
     *
     * @access  protected
     * @param   string  $lexeme    Name of the lexeme.
     * @param   string  $regex     Regular expression describing the lexeme.
     * @param   int     $offset    Offset.
     * @return  array
     * @throw   \Hoa\Compiler\Exception\Lexer
     */
    protected function matchLexeme ( $lexeme, $regex, $offset ) {

        $_regex = str_replace('#', '\#', $regex);
        $preg   = preg_match(
            '#(?|' . $_regex . ')#u',
            $this->_text,
            $matches,
            PREG_OFFSET_CAPTURE,
            $offset
        );

        if(0 === $preg || $offset !== $matches[0][1])
            return null;

        if('' === $matches[0])
            throw new \Hoa\Compiler\Exception\Lexer(
                'A lexeme must not match an empty value, which is the ' .
                'case of "%s" (%s).', 3, array($lexeme, $regex));

        return array(
            'token'  => $lexeme,
            'value'  => $matches[0][0],
            'length' => mb_strlen($matches[0][0])
        );
    }

    protected function generateOffSideTokens( $nextToken, $offset ) {
        $stackLength = count($this->_indentStack);
        $currentIndent = $this->_indentStack[$stackLength -1];
        $offsideTokens = array();
        $pos = 0;
        $newLineLength=0;
        $spaces = str_split($nextToken['value']);
        if ("\r" === $spaces[0]) {
            $newLineLength++;
            array_shift($spaces);
        }
        if ("\n" === $spaces[0]) {
            $newLineLength++;
            array_shift($spaces);
        }

        foreach($spaces as $space){
            if (' ' === $space) {
                $pos++;
                continue;
            }
            if("\t" === $space){
                $pos = ( floor($pos/$this->_options['offSideRules:tabStop'])+1 ) * $this->_options['offSideRules:tabStop'];
                continue;
            }
            throw new \Hoa\Compiler\Exception\UnrecognizedIndentToken(
                'Unrecognized indent token "%s" at line 1 and column %d:' .
                "\n" . '%s' . "\n" .
                str_repeat(' ', mb_strlen(substr($this->_text, 0, $offset))+$pos) . '↑',
                0, array(
                    mb_substr(substr($this->_text, $offset + $newLineLength + $pos), 0, 1),
                    $offset + $newLineLength + $pos,
                    $space //FIXME: give hex code in addition to non printable representation
                ), 1, $offset + $newLineLength + $pos);
        }
        if($pos === $currentIndent){
            $offsideTokens[] = array(
                'token' => self::SAMEINDENT,
                'value' => '',
                'length' => 0,
                'namespace' => 'default',
                'keep' => $this->_options['offSideRules:keepSameIndent'],
                'offset' => $offset
            );
        } elseif($pos > $currentIndent){
            $this->_indentStack[] = $pos;
            $offsideTokens[] = array(
                'token' => self::INDENT,
                'value' => '',
                'length' => 0,
                'namespace' => 'default',
                'keep' => true,
                'offset' => $offset
            );
        } else {
            do{
                array_pop($this->_indentStack);
                $stackLength--;
                if (0 === $stackLength)
                    throw new \Hoa\Compiler\Exception\UnrecognizedIndentToken(
                        'Dedent token "%s" at line 1 and column %d does not match previous indent levels:' .
                        "\n" . '%s' . "\n" .
                        str_repeat(' ', mb_strlen(substr($this->_text, 0, $offset))) . '↑',
                        0, array(
                            mb_substr(substr($this->_text, $offset), 0, 1),
                            $offset + 1,
                            $nextToken['value'] //FIXME: give hex code in addition to non printable representation
                        ), 1, $offset);

                $matchingIndent = $this->_indentStack[$stackLength - 1];

                $offsideTokens[] = array(
                    'token' => self::DEDENT,
                    'value' => '',
                    'length' => 0,
                    'namespace' => 'default',
                    'keep' => true,
                    'offset' => $offset
                );

                if ($pos === $matchingIndent)
                    break;

            } while(0 < $stackLength);
        }

        return $offsideTokens;
    }
}

}
