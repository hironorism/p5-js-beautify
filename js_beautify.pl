#!perl
use strict;
use warnings;

my ($input, @output, $token_text, $last_type, $last_text, $last_last_text, $last_word, $flags, @flag_store, $indent_string);
my ($whitespace, $wordchar, $punct, $parser_pos, $line_starters, $digits);
my ($prefix, $token_type, $do_block_just_closed);
my ($wanted_newline, $just_added_newline, $n_newlines);
my $preindent_string = '';

my ($opt_indent_size, $opt_indent_char, $opt_preserve_newlines, $opt_max_preserve_newlines, $opt_jslint_happy, $opt_keep_array_indentation, $opt_space_before_conditional, $opt_indent_case);
my $input_length;

#die "no such file $ARGV[0]" unless -e $ARGV[0];
print js_beautify( do { local $/; <> } );
#print js_beautify( 'function hoge(a,b,c){if(a){var hoge=100}else if(b){var hoge=1000}else{ var hoge = 10000; }}' ), $/;
             
sub js_beautify {
    my ($js_source_text, $options) = @_;

    $options ||= {};

    my $opt_brace_style;

    if (!defined $options->{space_after_anon_function}  && !defined $options->{jslint_happy}) {
        $options->{jslint_happy} = $options->{space_after_anon_function};
    }
    if (!defined $options->{braces_on_own_line}) {
        $opt_brace_style =  $options->{braces_on_own_line} ? "epand" : "collapse";;
    }
    $opt_brace_style = $options->{brace_style} ? $options->{brace_style} 
                     : $opt_brace_style ? $opt_brace_style 
                     : "collapse";

    $opt_indent_size              = $options->{indent_size} || 4;
    $opt_indent_char              = $options->{indent_char} || ' ';
    $opt_preserve_newlines        = !defined $options->{preserve_newlines}        ? 1 : $options->{preserve_newlines};
    $opt_max_preserve_newlines    = !defined $options->{max_preserve_newlines}    ? 0 : $options->{max_preserve_newlines};
    $opt_jslint_happy             = !defined $options->{jslint_happy}             ? 0 : $options->{jslint_happy};
    $opt_keep_array_indentation   = !defined $options->{keep_array_indentation}   ? 0 : $options->{keep_array_indentation};
    $opt_space_before_conditional = !defined $options->{space_before_conditional} ? 1 : $options->{space_before_conditional};
    $opt_indent_case              = !defined $options->{indent_case }             ? 0 : $options->{indent_case};

    $just_added_newline = 0;
    $input_length = length $js_source_text;

    $indent_string = '';
    while ($opt_indent_size > 0) {
        $indent_string .= $opt_indent_char;
        $opt_indent_size -= 1;
    }

    my @js_source_text = split //, $js_source_text; # add for translate
    while (scalar @js_source_text && ($js_source_text[0] eq ' ' || $js_source_text[0] eq '\t')) {
        $preindent_string .= shift @js_source_text;
    }
    $input = join '', @js_source_text;

    $last_word      = '';              # last 'TK_WORD' passed
    $last_type      = 'TK_START_EXPR'; # last token type
    $last_text      = '';              # last token text
    $last_last_text = '';              # pre-last token text
    @output         = ();

    $do_block_just_closed = 0;

    $whitespace = [ split //, "\n\r\t " ];
    $wordchar   = [ split //, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$' ];
    $digits     = [ split //, '0123456789' ];

    $punct  = '+ - * / % & ++ -- = += -= *= /= %= == === != !== > < >= <= >> << >>> >>>= >>= <<= && &= | || ! !! , : ? ^ ^= |= ::';
    $punct .= ' <%= <% %> <?= <? ?>'; # try to be a good boy and try not to break the markup language identifiers
    $punct  = [ split / /, $punct ];

    # words which should always start on new line.
    $line_starters = [ split /,/, 'continue,try,throw,return,var,if,switch,case,default,for,while,break,function' ];

    # states showing if we are currently in expression (i.e. "if" case) - 'EXPRESSION', or in usual block (like, procedure), 'BLOCK'.
    # some formatting depends on that.
    @flag_store = ();
    set_mode('BLOCK');

    $parser_pos = 0;
    while (1) {
        my $t = get_next_token($parser_pos);
        $token_text = $t->[0];
        $token_type = $t->[1];
#warn "$token_type:$token_text";
#warn "@output";
        if ($token_type eq 'TK_EOF') {
            last;
        }

        if ($token_type eq  'TK_START_EXPR') {

            if ($token_text eq '[') {

                if ($last_type eq 'TK_WORD' || $last_text eq ')') {
                    # this is array index specifier, break immediately
                    # a[x], fn()[x]
                    if (in_array($last_text, $line_starters)) {
                        print_single_space();
                    }
                    set_mode('(EXPRESSION)');
                    print_token();
                    next;
                }

                if ($flags->{mode} eq '[EXPRESSION]' || $flags->{mode} eq '[INDENTED-EXPRESSION]') {
                    if ($last_last_text eq ']' && $last_text eq',') {
                        # ], [ goes to new line
                        if ($flags->{mode} eq '[EXPRESSION]') {
                            $flags->{mode} = '[INDENTED-EXPRESSION]';
                            if (!$opt_keep_array_indentation) {
                                indent();
                            }
                        }
                        set_mode('[EXPRESSION]');
                        if (!$opt_keep_array_indentation) {
                            print_newline();
                        }
                    } elsif ($last_text eq '[') {
                        if ($flags->{mode} eq '[EXPRESSION]') {
                            $flags->{mode} = '[INDENTED-EXPRESSION]';
                            if (!$opt_keep_array_indentation) {
                                indent();
                            }
                        }
                        set_mode('[EXPRESSION]');

                        if (!$opt_keep_array_indentation) {
                            print_newline();
                        }
                    } else {
                        set_mode('[EXPRESSION]');
                    }
                } else {
                    set_mode('[EXPRESSION]');
                }

            } else {
                if ($last_word eq 'for') {
                    set_mode('(FOR-EXPRESSION)');
                } elsif (in_array($last_word, ['if', 'while'])) {
                    set_mode('(COND-EXPRESSION)');
                } else {
                    set_mode('(EXPRESSION)');
                }
            }

            if ($last_text eq ';' || $last_type eq 'TK_START_BLOCK') {
                print_newline();
            } elsif ($last_type eq 'TK_END_EXPR' || $last_type eq 'TK_START_EXPR' || $last_type eq 'TK_END_BLOCK' || $last_text eq '.') {
                if ($wanted_newline) {
                    print_newline();
                }
                # do nothing on (( and )( and ][ and ]( and .(
            } elsif ($last_type ne 'TK_WORD' && $last_type ne 'TK_OPERATOR') {
                print_single_space();
            } elsif ($last_word ne 'function' || $last_word eq 'typeof') {
                # function() vs function ()
                if ($opt_jslint_happy) {
                    print_single_space();
                }
            } elsif (in_array($last_text, $line_starters) || $last_text eq 'catch') {
                if ($opt_space_before_conditional) {
                    print_single_space();
                }
            }
            print_token();

            next;
        }
        elsif ($token_type eq 'TK_END_EXPR' ) {
            if ($token_text eq ']') {
                if ($opt_keep_array_indentation) {
                    if ($last_text eq '}') {
                        # trim_output();
                        # print_newline(true);
                        remove_indent();
                        print_token();
                        restore_mode();
                        next;
                    }
                } else {
                    if ($flags->{mode} eq '[INDENTED-EXPRESSION]') {
                        if ($last_text eq ']') {
                            restore_mode();
                            print_newline();
                            print_token();
                            next;
                        }
                    }
                }
            }
            restore_mode();
            print_token();
            next;
        }
        elsif ($token_type eq 'TK_START_BLOCK') {

            if ($last_word eq 'do') {
                set_mode('DO_BLOCK');
            } else {
                set_mode('BLOCK');
            }
            if ($opt_brace_style eq "expand" || $opt_brace_style eq "expand-strict") {
                my $empty_braces = 0;
                if ($opt_brace_style eq "expand-strict") {
                    $empty_braces = (look_up() eq '}');
                    if (!$empty_braces) {
                        print_newline(1);
                    }
                } else {
                    if ($last_type ne'TK_OPERATOR') {
                        if ($last_text eq '=' || (is_special_word($last_text) && $last_text ne 'else')) {
                            print_single_space();
                        } else {
                            print_newline(1);
                        }
                    }
                }
                print_token();
                if (!$empty_braces) { indent() };
            } else {
                if ($last_type ne 'TK_OPERATOR' && $last_type ne 'TK_START_EXPR') {
                    if ($last_type eq 'TK_START_BLOCK') {
                        print_newline();
                    } else {
                        print_single_space();
                    }
                } else {
                    # if TK_OPERATOR or TK_START_EXPR
                    if (is_array($flags->{previous_mode}) && $last_text eq ',') {
                        if ($last_last_text eq '}') {
                            # }, { in array context
                            print_single_space();
                        } else {
                            print_newline(); # [a, b, c, {
                        }
                    }
                }
                indent();
                print_token();
            }
            next;
        }
        elsif ($token_type eq 'TK_END_BLOCK') {
            restore_mode();
            if ($opt_brace_style eq "expand" || $opt_brace_style eq "expand-strict") {
                if ($last_text ne '{') {
                    print_newline();
                }
                print_token();
            } else {
                if ($last_type eq 'TK_START_BLOCK') {
                    # nothing
                    if ($just_added_newline) {
                        remove_indent();
                    } else {
                        # {}
                        trim_output();
                    }
                } else {
                    if (is_array($flags->{mode}) && $opt_keep_array_indentation) {
                        # we REALLY need a newline here, but newliner would skip that
                        $opt_keep_array_indentation = 0;
                        print_newline();
                        $opt_keep_array_indentation = 1;

                    } else {
                        print_newline();
                    }
                }
                print_token();
            }
            next;
        }
        elsif ($token_type eq 'TK_WORD') {

            # no, it's not you. even I have problems understanding how this works
            # and what does what.
            if ($do_block_just_closed) {
                # do {} ## while ()
                print_single_space();
                print_token();
                print_single_space();
                $do_block_just_closed = 0;
                next;
            }

            if ($token_text eq 'function') {
                if ($flags->{var_line}) {
                    $flags->{var_line_reindented} = 1;
                }
                if (($just_added_newline || $last_text eq ';') && $last_text ne '{'
                && $last_type ne 'TK_BLOCK_COMMENT' && $last_type ne 'TK_COMMENT') {
                    # make sure there is a nice clean space of at least one blank line
                    # before a new function definition
                    $n_newlines = $just_added_newline ? $n_newlines : 0;
                    if ( !$opt_preserve_newlines) {
                        $n_newlines = 1;
                    }

                    for (my $i = 0; $i < 2 - $n_newlines; $i++) {
                        print_newline(0);
                    }
                }
            }

            if ($token_text eq 'case' || $token_text eq 'default') {
                if ($last_text eq ':' || $flags->{case_body}) {
                    # switch cases following one another
                    remove_indent();
                } else {
                    # case statement starts in the same line where switch
                    if (!$opt_indent_case) {
                        $flags->{indentation_level}--;
                    }
                    print_newline();
                    if (!$opt_indent_case) {
                        $flags->{indentation_level}++;
                    }
                }
                print_token();
                $flags->{in_case}   = 1;
                $flags->{case_body} = 0;
                next;
            }

            $prefix = 'NONE';

            if ($last_type eq 'TK_END_BLOCK') {

                if (!in_array(lc $token_text, ['else', 'catch', 'finally'])) {
                    $prefix = 'NEWLINE';
                } else {
                    if ($opt_brace_style eq "expand" || $opt_brace_style eq "end-expand" || $opt_brace_style eq "expand-strict") {
                        $prefix = 'NEWLINE';
                    } else {
                        $prefix = 'SPACE';
                        print_single_space();
                    }
                }
            } elsif ($last_type eq 'TK_SEMICOLON' && ($flags->{mode} eq 'BLOCK' || $flags->{mode} eq 'DO_BLOCK')) {
                $prefix = 'NEWLINE';
            } elsif ($last_type eq 'TK_SEMICOLON' && is_expression($flags->{mode})) {
                $prefix = 'SPACE';
            } elsif ($last_type eq 'TK_STRING') {
                $prefix = 'NEWLINE';
            } elsif ($last_type eq 'TK_WORD') {
                if ($last_text eq 'else') {
                    # eat newlines between ...else *** some_op...
                    # won't preserve extra newlines in this place (if any), but don't care that much
                    trim_output(0);
                }
                $prefix = 'SPACE';
            } elsif ($last_type eq 'TK_START_BLOCK') {
                $prefix = 'NEWLINE';
            } elsif ($last_type eq 'TK_END_EXPR') {
                print_single_space();
                $prefix = 'NEWLINE';
            }

            if (in_array($token_text, $line_starters) && $last_text ne ')') {
                if ($last_text eq 'else') {
                    $prefix = 'SPACE';
                } else {
                    $prefix = 'NEWLINE';
                }

                if ($token_text eq 'function' && ($last_text eq 'get' || $last_text eq 'set')) {
                    $prefix = 'SPACE';
                }
            }

            if ($flags->{if_line} && $last_type eq 'TK_END_EXPR') {
                $flags->{if_line} = 0;
            }
            if (in_array(lc $token_text, ['else', 'catch', 'finally'])) {
                if ($last_type ne 'TK_END_BLOCK' || $opt_brace_style eq "expand" || $opt_brace_style eq "end-expand" || $opt_brace_style eq "expand-strict") {
                    print_newline();
                } else {
                    trim_output(1);
                    print_single_space();
                }
            } elsif ($prefix eq 'NEWLINE') {
                if (($last_type eq 'TK_START_EXPR' || $last_text eq '=' || $last_text eq ',') && $token_text eq 'function') {
                    # no need to force newline on 'function': (function
                    # DONOTHING
                } elsif ($token_text eq 'function' && $last_text eq 'new') {
                    print_single_space();
                } elsif (is_special_word($last_text)) {
                    # no newline between 'return nnn'
                    print_single_space();
                } elsif ($last_type ne 'TK_END_EXPR') {
                    if (($last_type ne 'TK_START_EXPR' || $token_text ne 'var') && $last_text ne ':') {
                        # no need to force newline on 'var': for (var x = 0...)
                        if ($token_text eq 'if' && $last_word eq 'else' && $last_text ne '{') {
                            # no newline for } else if {
                            print_single_space();
                        } else {
                            $flags->{var_line} = 0;
                            $flags->{var_line_reindented} = 0;
                            print_newline();
                        }
                    }
                } elsif (in_array($token_text, $line_starters) && $last_text ne ')') {
                    $flags->{var_line} = 0;
                    $flags->{var_line_reindented} = 0;
                    print_newline();
                }
            } elsif (is_array($flags->{mode}) && $last_text eq ',' && $last_last_text eq '}') {
                print_newline(); # }, in lists get a newline treatment
            } elsif ($prefix eq 'SPACE') {
                print_single_space();
            }
            print_token();
            $last_word = $token_text;

            if ($token_text eq 'var') {
                $flags->{var_line} = 1;
                $flags->{var_line_reindented} = 0;
                $flags->{var_line_tainted} = 0;
            }

            if ($token_text eq 'if') {
                $flags->{if_line} = 1;
            }
            if ($token_text eq 'else') {
                $flags->{if_line} = 0;
            }

            next;
        }
        elsif ($token_type eq 'TK_SEMICOLON') {

            print_token();
            $flags->{var_line} = 0;
            $flags->{var_line_reindented} = 0;
            if ($flags->{mode} eq 'OBJECT') {
                # OBJECT mode is weird and doesn't get reset too well.
                $flags->{mode} = 'BLOCK';
            }
            next;

        }
        elsif ($token_type eq 'TK_STRING') {

            if ($last_type eq 'TK_END_EXPR' && in_array($flags->{previous_mode}, ['(COND-EXPRESSION)', '(FOR-EXPRESSION)'])) {
                print_single_space();
            } elsif ($last_type eq 'TK_STRING' || $last_type eq 'TK_START_BLOCK' || $last_type eq 'TK_END_BLOCK' || $last_type eq 'TK_SEMICOLON') {
                print_newline();
            } elsif ($last_type eq 'TK_WORD') {
                print_single_space();
            }
            print_token();
            next;

        }
        elsif ($token_type eq 'TK_EQUALS') {
            if ($flags->{var_line}) {
                # just got an '=' in a var-line, different formatting/line-breaking, etc will now be done
                $flags->{var_line_tainted} = 1;
            }
            print_single_space();
            print_token();
            print_single_space();
            next;

        }
        elsif ($token_type eq 'TK_OPERATOR') {

            my $space_before = 1;
            my $space_after  = 1;

            if ($flags->{var_line} && $token_text eq ',' && (is_expression($flags->{mode}))) {
                # do not break on comma, for(var a = 1, b = 2)
                $flags->{var_line_tainted} = 0;
            }

            if ($flags->{var_line}) {
                if ($token_text eq ',') {
                    if ($flags->{var_line_tainted}) {
                        print_token();
                        $flags->{var_line_reindented} = 1;
                        $flags->{var_line_tainted} = 0;
                        print_newline();
                        next;
                    } else {
                        $flags->{var_line_tainted} = 0;
                    }
                # } else if (token_text === ':') {
                    # hmm, when does this happen? tests don't catch this
                    # flags.var_line = false;
                }
            }

            if (is_special_word($last_text)) {
                # "return" had a special handling in TK_WORD. Now we need to return the favor
                print_single_space();
                print_token();
                next;
            }

            if ($token_text eq ':' && $flags->{in_case}) {
                if ($opt_indent_case) {
                    $flags->{case_body} = 1;
                }
                print_token(); # colon really asks for separate treatment
                print_newline();
                $flags->{in_case} = 0;
                next;
            }

            if ($token_text eq '::') {
                # no spaces around exotic namespacing syntax operator
                print_token();
                next;
            }

            if ($token_text eq ',') {
                if ($flags->{var_line}) {
                    if ($flags->{var_line_tainted}) {
                        print_token();
                        print_newline();
                        $flags->{var_line_tainted} = 0;
                    } else {
                        print_token();
                        print_single_space();
                    }
                } elsif ($last_type eq 'TK_END_BLOCK' && $flags->{mode} ne "(EXPRESSION)") {
                    print_token();
                    if ($flags->{mode} eq 'OBJECT' && $last_text eq '}') {
                        print_newline();
                    } else {
                        print_single_space();
                    }
                } else {
                    if ($flags->{mode} eq 'OBJECT') {
                        print_token();
                        print_newline();
                    } else {
                        # EXPR or DO_BLOCK
                        print_token();
                        print_single_space();
                    }
                }
                next;
            # } else if (in_array(token_text, ['--', '++', '!']) || (in_array(token_text, ['-', '+']) && (in_array(last_type, ['TK_START_BLOCK', 'TK_START_EXPR', 'TK_EQUALS']) || in_array(last_text, line_starters) || in_array(last_text, ['==', '!=', '+=', '-=', '*=', '/=', '+', '-'])))) {
            } elsif (
                in_array($token_text, ['--', '++', '!']) || 
                (in_array($token_text, ['-', '+']) && 
                (in_array($last_type, ['TK_START_BLOCK', 'TK_START_EXPR', 'TK_EQUALS', 'TK_OPERATOR']) || in_array($last_text, $line_starters)))
            ) {
                # unary operators (and binary +/- pretending to be unary) special cases

                $space_before = 0;
                $space_after = 0;

                if ($last_text eq ';' && is_expression($flags->{mode})) {
                    # for (;; ++i)
                    #        ^^^
                    $space_before = 1;
                }
                if ($last_type eq 'TK_WORD' && in_array($last_text, $line_starters)) {
                    $space_before = 1;
                }

                if ($flags->{mode} eq 'BLOCK' && ($last_text eq '{' || $last_text eq ';')) {
                    # { foo; --i }
                    # foo(); --bar;
                    print_newline();
                }
            } elsif ($token_text eq '.') {
                # decimal digits or object.property
                $space_before = 0;

            } elsif ($token_text eq ':') {
                if ($flags->{ternary_depth} == 0) {
                    $flags->{mode} = 'OBJECT';
                    $space_before = 0;
                } else {
                    $flags->{ternary_depth} -= 1;
                }
            } elsif ($token_text eq '?') {
                $flags->{ternary_depth} += 1;
            }
            if ($space_before) {
                print_single_space();
            }

            print_token();

            if ($space_after) {
                print_single_space();
            }

            if ($token_text eq '!') {
                # flags.eat_next_space = true;
            }

            next;
        }
        elsif ($token_type eq 'TK_BLOCK_COMMENT') {

            my @lines = split /\x0a|\x0d\x0a/, $token_text;
            shift @lines; # lines.slice(1)
            if (all_lines_start_with(\@lines, '*')) {
                # javadoc: reformat and reindent
                print_newline();
                push @output, $lines[0];
                for (my $i = 1; $i < scalar @lines; $i++) {
                    print_newline();
                    push @output, ' ';
                    push @output, trim($lines[$i]);
                }

            } else {

                # simple block comment: leave intact
                if (scalar @lines > 1) {
                    # multiline comment block starts with a new line
                    print_newline();
                } else {
                    # single-line /* comment */ stays where it is
                    if ($last_type eq 'TK_END_BLOCK') {
                        print_newline();
                    } else {
                        print_single_space();
                    }

                }

                for (my $i = 0; $i < scalar @lines; $i++) {
                    push @output, $lines[$i];
                    push @output, '\n';
                }

            }
            if(look_up('\n') ne '\n') {
                print_newline();
            }
            next;
        }
        elsif ($token_type eq 'TK_INLINE_COMMENT') {
            print_single_space();
            print_token();
            if (is_expression($flags->{mode})) {
                print_single_space();
            } else {
                force_newline();
            }
            next;
        }
        elsif ($token_type eq 'TK_COMMENT') {

            # print_newline();
            if ($wanted_newline) {
                print_newline();
            } else {
                print_single_space();
            }
            print_token();
            if(look_up('\n') ne '\n') {
                force_newline();
            }
            next;
        }
        elsif ($token_type eq 'TK_UNKNOWN') {
            if (is_special_word($last_text)) {
                print_single_space();
            }
            print_token();
            next;
        }

        $last_last_text = $last_text;
        $last_type = $token_type;
        $last_text = $token_text;
    }

    my $sweet_code = $preindent_string . join '', @output;
#       $sweet_code =~ s/[\n ]+$//;
    return $sweet_code;
}

sub trim_output {
    my ($eat_newlines) = @_;
    $eat_newlines = !defined $eat_newlines ? 0 : $eat_newlines;
    while (scalar @output && ($output[scalar @output - 1] eq ' '
        || $output[scalar @output - 1] eq $indent_string
        || $output[scalar @output - 1] eq $preindent_string
        || ($eat_newlines && ($output[scalar @output - 1] eq '\n' || $output[scalar @output - 1] eq '\r')))) {
        pop @output;
    }
}

sub trim {
    my ($s) = @_;
    $s =~ s///;
    $s =~ s/^\s\s*|\s\s*$//;
    return $s;
}


sub force_newline {
    my $old_keep_array_indentation = $opt_keep_array_indentation;
    $opt_keep_array_indentation = 0;
    print_newline();
    $opt_keep_array_indentation = $old_keep_array_indentation;
}


sub print_newline {
    my ($ignore_repeated) = @_;

    $flags->{eat_next_space} = 0;
    if ($opt_keep_array_indentation && is_array($flags->{mode})) {
        return;
    }

    $ignore_repeated = !defined $ignore_repeated ? 1 : $ignore_repeated;

    $flags->{if_line} = 0;
    trim_output();

    if (!scalar @output) {
        return; # no newline on start of file
    }

    if ($output[scalar @output - 1] ne "\n" || !$ignore_repeated) {
        $just_added_newline = 1;
        push @output, "\n";
    }
    if ($preindent_string) {
        push @output, $preindent_string;
    }
    for (my $i = 0; $i < $flags->{indentation_level}; $i++) {
        push @output, $indent_string;
    }
    if ($flags->{var_line} && $flags->{var_line_reindented}) {
        push @output, $indent_string; # skip space-stuffing, if indenting with a tab
    }
    if ($flags->{case_body}) {
        push @output, $indent_string;
    }
}

sub print_single_space {

    if ($last_type eq 'TK_COMMENT') {
        # no you will not print just a space after a comment
        return print_newline(1);
    }

    if ($flags->{eat_next_space}) {
        $flags->{eat_next_space} = 0;
        return;
    }
    my $last_output = ' ';
    if (scalar @output) {
        $last_output = $output[ scalar @output - 1];
    }
    if ($last_output ne ' ' && $last_output ne '\n' && $last_output ne $indent_string) { # prevent occassional duplicate space
        push @output, ' ';
    }
}


sub print_token {
    $just_added_newline = 0;
    $flags->{eat_next_space} = 0;
    push @output, $token_text;
}

sub indent {
    $flags->{indentation_level} += 1;
}

sub remove_indent {
    if (scalar @output && $output[scalar @output - 1] eq $indent_string) {
        pop @output;
    }
}

sub set_mode {
    my ($mode) = @_;
    if ($flags) {
        push @flag_store, $flags;
    }
    $flags = {
        previous_mode        => $flags ? $flags->{mode} : 'BLOCK',
        mode                 => $mode,
        var_line             => 0,
        var_line_tainted     => 0,
        var_line_reindented  => 0,
        in_html_comment      => 0,
        if_line              => 0,
        in_case              => 0,
        case_body            => 0,
        eat_next_space       => 0,
        indentation_baseline => -1,
        indentation_level    => $flags ? $flags->{indentation_level} 
                                           + ($flags->{case_body}?1:0) 
                                           + (($flags->{var_line} && $flags->{var_line_reindented}) ? 1 : 0) 
                                       : 0,
        ternary_depth        => 0
    };
}


sub is_array {
    my ($mode) = @_;
    return $mode eq '[EXPRESSION]' || $mode eq '[INDENTED-EXPRESSION]';
}

sub is_expression {
    my ($mode) = @_;
    return in_array($mode, ['[EXPRESSION]', '(EXPRESSION)', '(FOR-EXPRESSION)', '(COND-EXPRESSION)']);
}

sub restore_mode {
    $do_block_just_closed = $flags->{mode} eq 'DO_BLOCK';
    if (scalar @flag_store > 0) {
        my $mode = $flags->{mode};
        $flags = pop @flag_store;
        $flags->{previous_mode} = $mode;
    }
}

sub all_lines_start_with {
    my ($lines, $c) = @_;
    for (my $i = 0; $i < scalar @{ $lines }; $i++) {
        if (substr(trim($lines->[$i]), 0, 1) ne $c) {
#        if (trim($lines->[$i])[0] ne $c) {
            return 0;
        }
    }
    return 1;
}

sub is_special_word {
    my ($word) = @_;
    return in_array($word, ['case', 'return', 'do', 'if', 'throw', 'else']);
}

sub in_array {
    my ($what, $arr) = @_;
    for (my $i = 0; $i < scalar @{$arr}; $i++) {
        if ($arr->[$i] eq $what) {
            return 1;
        }
    }
    return 0;
}


sub look_up {
    my ($exclude) = @_;
    my $local_pos = $parser_pos;
    my $c = substr($input, $local_pos, 1);
    while (in_array($c, $whitespace) && $c ne $exclude) {
        $local_pos++;
        if ($local_pos >= $input_length) { return 0 }
        $c = substr($input, $local_pos, 1);
    }
    return $c;
}

sub get_next_token {
    $n_newlines = 0;

    if ($parser_pos >= $input_length) {
        return ['', 'TK_EOF'];
    }

    $wanted_newline = 0;

    my $c = substr($input, $parser_pos, 1);
    $parser_pos += 1;

    my $keep_whitespace = $opt_keep_array_indentation && is_array($flags->{mode});

    if ($keep_whitespace) {

        #
        # slight mess to allow nice preservation of array indentation and reindent that correctly
        # first time when we get to the arrays:
        # var a = [
        # ....'something'
        # we make note of whitespace_count = 4 into flags.indentation_baseline
        # so we know that 4 whitespaces in original source match indent_level of reindented source
        #
        # and afterwards, when we get to
        #    'something,
        # .......'something else'
        # we know that this should be indented to indent_level + (7 - indentation_baseline) spaces
        #
        my $whitespace_count = 0;

        while (in_array($c, $whitespace)) {

            if ($c eq "\n") {
                trim_output();
                push @output, "\n";
                $just_added_newline = 1;
                $whitespace_count = 0;
            } else {
                if ($c eq '\t') {
                    $whitespace_count += 4;
                } elsif ($c eq '\r') {
                    # nothing
                } else {
                    $whitespace_count += 1;
                }
            }

            if ($parser_pos >= $input_length) {
                return ['', 'TK_EOF'];
            }

            $c = substr($input, $parser_pos, 1);
            $parser_pos += 1;

        }
        if ($flags->{indentation_baseline} == -1) {
            $flags->{indentation_baseline} = $whitespace_count;
        }

        if ($just_added_newline) {
            my $i;
            for ($i = 0; $i < $flags->{indentation_level} + 1; $i++) {
                push @output, $indent_string;
            }
            if ($flags->{indentation_baseline} != -1) {
                for ($i = 0; $i < $whitespace_count - $flags->{indentation_baseline}; $i++) {
                    push @output, ' ';
                }
            }
        }

    } else {
        while (in_array($c, $whitespace)) {

            if ($c eq "\n") {
                $n_newlines += ( ($opt_max_preserve_newlines) ? ($n_newlines <= $opt_max_preserve_newlines) ? 1: 0: 1 );
            }

            if ($parser_pos >= $input_length) {
                return ['', 'TK_EOF'];
            }

            $c = substr($input, $parser_pos, 1);
            $parser_pos += 1;

        }

        if ($opt_preserve_newlines) {
            if ($n_newlines > 1) {
                for (my $i = 0; $i < $n_newlines; $i++) {
                    print_newline($i == 0);
                    $just_added_newline = 1;
                }
            }
        }
        $wanted_newline = $n_newlines > 0;
    }


    if (in_array($c, $wordchar)) {
        if ($parser_pos < $input_length) {
            while (in_array(substr($input, $parser_pos, 1), $wordchar)) {
                $c .= substr($input, $parser_pos, 1);
                $parser_pos += 1;
                if ($parser_pos == $input_length) {
                    last;
                }
            }
        }

        # small and surprisingly unugly hack for 1E-10 representation
        if ($parser_pos != $input_length && $c =~ m/^[0-9]+[Ee]$/ && (substr($input, $parser_pos, 1) eq '-' || substr($input, $parser_pos, 1) eq '+')) {

            my $sign = substr($input, $parser_pos, 1);
            $parser_pos += 1;

            my $t = get_next_token($parser_pos);
            $c .= $sign . $t->[0];
            return [$c, 'TK_WORD'];
        }

        if ($c eq'in') { # hack for 'in' operator
            return [$c, 'TK_OPERATOR'];
        }
        if ($wanted_newline && $last_type ne 'TK_OPERATOR'
            && $last_type ne 'TK_EQUALS'
            && !$flags->{if_line} && ($opt_preserve_newlines || $last_text ne 'var')) {
            print_newline();
        }
        return [$c, 'TK_WORD'];
    }

    if ($c eq '(' || $c eq '[') {
        return [$c, 'TK_START_EXPR'];
    }

    if ($c eq ')' || $c eq ']') {
        return [$c, 'TK_END_EXPR'];
    }

    if ($c eq '{') {
        return [$c, 'TK_START_BLOCK'];
    }

    if ($c eq '}') {
        return [$c, 'TK_END_BLOCK'];
    }

    if ($c eq ';') {
        return [$c, 'TK_SEMICOLON'];
    }

    if ($c eq '/') {
        my $comment = '';
        # peek for comment /* ... */
        my $inline_comment = 1;
        if (substr($input, $parser_pos, 1) eq '*') {
            $parser_pos += 1;
            if ($parser_pos < $input_length) {
                while (
                    ! (substr($input, $parser_pos, 1) eq '*' && substr($input, ($parser_pos + 1), 1) && substr($input, ($parser_pos + 1), 1) eq '/') && 
                    $parser_pos < $input_length 
                ) {
                    $c = substr($input, $parser_pos, 1);
                    $comment .= $c;
                    if ($c eq '\x0d' || $c eq '\x0a') {
                        $inline_comment = 0;
                    }
                    $parser_pos += 1;
                    if ($parser_pos >= $input_length) {
                        last;;
                    }
                }
            }
            $parser_pos += 2;
            if ($inline_comment && $n_newlines == 0) {
                return ['/*' . $comment . '*/', 'TK_INLINE_COMMENT'];
            } else {
                return ['/*' . $comment . '*/', 'TK_BLOCK_COMMENT'];
            }
        }
        # peek for comment // ...
        if (substr($input, $parser_pos, 1) eq '/') {
            $comment = $c;
            while (substr($input, $parser_pos, 1) ne '\r' && substr($input, $parser_pos, 1) ne '\n') {
                $comment .= substr($input, $parser_pos, 1);
                $parser_pos += 1;
                if ($parser_pos >= $input_length) {
                    last;
                }
            }
            $parser_pos += 1;
            if ($wanted_newline) {
                print_newline();
            }
            return [$comment, 'TK_COMMENT'];
        }

    }

    if ($c eq "'" || # string
        $c eq '"' || # string
       ($c eq '/' &&
        (($last_type eq 'TK_WORD' && is_special_word($last_text)) ||
         ($last_text eq ')' && in_array($flags->{previous_mode}, ['(COND-EXPRESSION)', '(FOR-EXPRESSION)'])) ||
         ($last_type eq 'TK_COMMENT' || 
          $last_type eq 'TK_START_EXPR' || 
          $last_type eq 'TK_START_BLOCK' || 
          $last_type eq 'TK_END_BLOCK' || 
          $last_type eq 'TK_OPERATOR' || 
          $last_type eq 'TK_EQUALS' || 
          $last_type eq 'TK_EOF' || 
          $last_type eq 'TK_SEMICOLON')))) { # regexp
        my $sep = $c;
        my $esc = 0;
        my $resulting_string = $c;

        if ($parser_pos < $input_length) {
            if ($sep eq '/') {
                #
                # handle regexp separately...
                #
                my $in_char_class = 0;
                while ($esc || $in_char_class || substr($input, $parser_pos, 1) ne $sep) {
                    $resulting_string .= substr($input, $parser_pos, 1);
                    if (!$esc) {
                        $esc = substr($input, $parser_pos, 1) eq '\\';
                        if (substr($input, $parser_pos, 1) eq '[') {
                            $in_char_class = 1;
                        } elsif (substr($input, $parser_pos, 1) eq ']') {
                            $in_char_class = 0;
                        }
                    } else {
                        $esc = 0;
                    }
                    $parser_pos += 1;
                    if ($parser_pos >= $input_length) {
                        # incomplete string/rexp when end-of-file reached.
                        # bail out with what had been received so far.
                        return [$resulting_string, 'TK_STRING'];
                    }
                }

            } else {
                #
                # and handle string also separately
                #
                while ($esc || substr($input, $parser_pos, 1) ne $sep) {
                    $resulting_string .= substr($input, $parser_pos, 1);
                    if (!$esc) {
                        $esc = substr($input, $parser_pos, 1) eq '\\';
                    } else {
                        $esc = 0;
                    }
                    $parser_pos += 1;
                    if ($parser_pos >= $input_length) {
                        # incomplete string/rexp when end-of-file reached.
                        # bail out with what had been received so far.
                        return [$resulting_string, 'TK_STRING'];
                    }
                }
            }

        }

        $parser_pos += 1;

        $resulting_string .= $sep;

        if ($sep eq '/') {
            # regexps may have modifiers /regexp/MOD , so fetch those, too
            while ($parser_pos < $input_length && in_array(substr($input, $parser_pos, 1), $wordchar)) {
                $resulting_string .= substr($input, $parser_pos, 1);
                $parser_pos += 1;
            }
        }
        return [$resulting_string, 'TK_STRING'];
    }

    if ($c eq '#') {

        if (scalar @output == 0 && substr($input, $parser_pos, 1) eq '!') {
            # shebang
            my $resulting_string = $c; # XXX
            while ($parser_pos < $input_length && $c ne '\n') {
                $c = substr($input, $parser_pos, 1);
                $resulting_string .= $c;
                $parser_pos += 1;
            }
            push @output, (trim($resulting_string) . '\n');
            print_newline();
            return get_next_token();
        }

        # Spidermonkey-specific sharp variables for circular references
        # https://developer.mozilla.org/En/Sharp_variables_in_JavaScript
        # http://mxr.mozilla.org/mozilla-central/source/js/src/jsscan.cpp around line 1935
        my $sharp = '#';
        if ($parser_pos < $input_length && in_array(substr($input, $parser_pos, 1), $digits)) {
            do {
                $c = substr($input, $parser_pos, 1);
                $sharp .= $c;
                $parser_pos += 1;
            } while ($parser_pos < $input_length && $c ne '#' && $c ne '=');
            if ($c eq '#') {
                # 
            } elsif (substr($input, $parser_pos, 1) eq '[' && substr($input, ($parser_pos + 1), 1) eq ']') {
                $sharp .= '[]';
                $parser_pos += 2;
            } elsif (substr($input, $parser_pos, 1) eq '{' && substr($input, ($parser_pos + 1), 1) eq '}') {
                $sharp .= '{}';
                $parser_pos += 2;
            }
            return [$sharp, 'TK_WORD'];
        }
    }

    if ($c eq '<' && substr($input, $parser_pos - 1, $parser_pos + 3) eq '<!--') {
        $parser_pos += 3;
        $c = '<!--';
        my @input = split //, $input; # add for translate
        while ($input[$parser_pos] ne '\n' && $parser_pos < $input_length) {
            $c .= $input[$parser_pos];
            $parser_pos++;
        }
        $flags->{in_html_comment} = 0;
        return [$c, 'TK_COMMENT'];
    }

    if ($c eq '-' && $flags->{in_html_comment} && substr($input, $parser_pos - 1, $parser_pos + 2) eq '-->') {
        $flags->{in_html_comment} = 0;
        $parser_pos += 2;
        if ($wanted_newline) {
            print_newline();
        }
        return ['-->', 'TK_COMMENT'];
    }

    if (in_array($c, $punct)) {
        while ($parser_pos < $input_length && in_array($c . substr($input, $parser_pos, 1), $punct)) {
            $c .= substr($input, $parser_pos, 1);
            $parser_pos += 1;
            if ($parser_pos >= $input_length) {
                last;
            }
        }

        if ($c eq '=') {
            return [$c, 'TK_EQUALS'];
        } else {
            return [$c, 'TK_OPERATOR'];
        }
    }

    return [$c, 'TK_UNKNOWN'];
}


