function mark
	if [ (count $argv) = 0 -o $argv[1] = '.' ]
		set MARK (basename $PWD)
	else
		set MARK $argv[1]
	end

	if read_confirm "Mark $PWD as $MARK?"
		mkdir -p $MARKPATH
		ln -s $PWD "$MARKPATH/$MARK"
	end
end
