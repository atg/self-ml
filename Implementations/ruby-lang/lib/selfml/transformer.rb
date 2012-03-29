require 'rubygems'
require 'parslet'

module SelfML

  class HashTransformer < Parslet::Transform
  	rule(:list => subtree(:l)) do
  		head = String(l[:head]).to_sym
  		tail = Array(l[:tail])
  
  		{ head => tail }
  	end
  	
    rule(:string   => simple(:s)) { String(s) }
    rule(:verbatim => simple(:s)) { String(s) }
  end
  

  class Transformer < Parslet::Transform
  	include AST

  	rule(:list => subtree(:l)) do
  		Node.new(
  			StringNode.new(l[:head]),
  			Array(l[:tail]))
  	end

    # Comments
  	rule(:comment => simple(:s)    ) { Comment.new(s)             }
    rule(:block   => simple(:blk)  ) { Comment.new(blk)           }
    rule(:block   => sequence(:blk)) { Comment.new(blk.reduce :+) }
    rule(:line    => simple(:ln)   ) { Comment.new(ln)            }
  	
  	# Strings
  	rule(:string    => simple(:s)) { StringNode.new(s) }
    rule(:verbatim  => simple(:s)) { StringNode.new(s) }
    
    rule(:backticks => sequence(:s)) { StringNode.new("#{s}") }
    rule(:ticks     =>   simple(:s)) { "`" }
    
    rule(:brackets => sequence(:b)) { StringNode.new("#{b}")   }
    rule(:nested   =>   simple(:n)) { StringNode.new(n)        }
    rule(:nested   => sequence(:n)) { StringNode.new("[#{n}]") }
    
    rule(:text => simple(:t)) { StringNode.new(t) }
  end

end