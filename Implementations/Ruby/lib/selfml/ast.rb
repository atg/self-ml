module SelfML::AST

  class Node
    attr_accessor :head , :tail
    
    def initialize( head , tail )
      @head , @tail = head , tail
    end
    
    def serialize
      tail = @tail.map do |t|
        case t
        when Node
          "\n    #{t.serialize}"
        when StringNode
          " #{t.serialize}"
        else
          ""
        end
      end
      
      "(#{head}#{tail})"
    end

  end
  
  class StringNode < String
    
    def serialize
      return "`#{self.gsub("`","``")}`" if self.count("[") != self.count("]")
      return "[#{self}]"                if self.match(/[#`()\[\]{} ]/)

      self
    end
    
  end
  
  class Comment < String
    def serialize; ""; end
  end
  
  class BlockComment < Comment
  end
  
  class LineComment < Comment
  end

end