require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    if !read_file_lines(path)
        return nil
    end
    board = GameBoard.new 10, 10
    valid = 0
    read_file_lines(path) do |line|
        if line =~ /\((\d+),(\d+)\), (Up|Down|Left|Right), ([1-5])/

            if(valid < 5)
                puts line
                pos = Position.new($1.to_i,$2.to_i)
                dir = $3
                siz = $4.to_i
                testShip = Ship.new(pos, dir, siz)
                board.add_ship(testShip)
                valid = valid + 1
            end
        end
    end
    if valid < 5
        return nil
    end
    return board
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    if !read_file_lines(path)
        return nil
    end
    positions = []
    read_file_lines(path) do |line|
        if line.match(/^\(([1-9]|10),([1-9]|10)\)/)
            positions.push(Position.new($1,$2))
        end
    end
    return positions
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
