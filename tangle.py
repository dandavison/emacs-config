import re


START_BLOCK = re.compile(r"^ *#\+begin_src (emacs-lisp|elisp) *$")
END_BLOCK = re.compile(r"^ *#\+end_src *$") #


if __name__ == '__main__':
    import sys
    in_block = False
    for line in sys.stdin:
        if START_BLOCK.match(line):
            assert not in_block
            in_block = True
            continue
        elif END_BLOCK.match(line):
            # We will often not be in a to-be-tangled emacs-lisp block.
            if in_block:
                sys.stdout.write('\n')
            in_block = False
        if in_block:
            sys.stdout.write(line)
