class PhoneBook
    def initialize
        @numbers = Hash.new
        @isListed = Hash.new
        @listed = []
    end

    def add(name, number, is_listed)
        added = false
        phone = /\d{3}-\d{3}-\d{4}/
        if !@numbers.key?(name)
            if number.match?(phone)
                if !is_listed || !@listed.include?(number)
                    @numbers[name] = number
                    @isListed[name] = is_listed
                    if is_listed
                       @listed.push(number) 
                    end
                    added = true
                end
            end
        end
        return added
    end

    def lookup(name)
        number = ""
        number = @numbers.fetch(name)
        if @listed.include?(number) && @isListed[name] == true
            return number
        end
        return nil
    end

    def lookupByNum(number)
        name = ""
        name = @numbers.key(number)
        if @listed.include?(number)
            return name
        end
        return nil
    end

    def namesByAc(areacode)
        area = []
        @numbers.each_pair do |name, number|
            if number[0,3].eql?(areacode)
                area.append(name)
            end
        end
        return area
    end

end