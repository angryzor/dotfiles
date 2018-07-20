function marks
	for link in $MARKPATH/*
		printf "%s\t-> %s \t\n" (set_color cyan)(basename $link)(set_color normal) (set_color blue)(readlink $link)(set_color normal)
	end
end

