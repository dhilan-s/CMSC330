class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @pieces = Array.new(max_row+1) { Array.new(max_column+1) { "-, -" } }
        @pieces[0][0] = "X"
        i = 1
        while i <= max_row
            @pieces[0][i] = "  #{i.to_s} "
            @pieces[i][0] = i.to_s
            i = i+1
        end
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        i = 0
        case ship.orientation
        when "Up"
            while i<ship.size
                if @pieces[ship.start_position.row-i][ship.start_position.column] != "-, -"
                    return false
                end
                i = i + 1
            end
            i = 0
            while i<ship.size
                @pieces[ship.start_position.row-i][ship.start_position.column] = "B, -"
                i = i + 1
            end
        when "Down"
            while i<ship.size
                if @pieces[ship.start_position.row+i][ship.start_position.column] != "-, -"
                    return false
                end
                i = i + 1
            end
            i = 0
            while i<ship.size
                @pieces[ship.start_position.row+i][ship.start_position.column] = "B, -"
                i = i + 1
            end
        when "Left"
            while i<ship.size
                if @pieces[ship.start_position.row][ship.start_position.column-i] != "-, -"
                    return false
                end
                i = i + 1
            end
            i = 0
            while i<ship.size
                @pieces[ship.start_position.row][ship.start_position.column-i] = "B, -"
                i = i + 1
            end
        when "Right"
            while i<ship.size
                if @pieces[ship.start_position.row][ship.start_position.column+i] != "-, -"
                    return false
                end
                i = i + 1
            end
            i = 0
            while i<ship.size
                @pieces[ship.start_position.row][ship.start_position.column+i] = "B, -"
                i = i + 1
            end
        end
        #to_s
        true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        if position.row <= @max_row && position.row <= @max_column
            # check position
            if @pieces[position.row][position.column] == "B, -" || @pieces[position.row][position.column] == "B, A"
                @pieces[position.row][position.column] = "B, A"
                return true
            end
            @pieces[position.row][position.column] = "-, A"
        end
        return false
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        success = 0
        @pieces.each do |row|
            row.each do |column|
                if column == "B, A"
                    success = success + 1 
                end
            end
        end
        return success
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        @pieces.each do |r|
            r.each do |c|
                if c == "B, -"
                    return false 
                end
            end
        end
        return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        @pieces.each do |r|
            puts r.each { |p| p }.join(" | ")
        end
    end
end
