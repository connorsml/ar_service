#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require "erlix"

module ARService

  def configure(sname="ruby", real_node="ar_service@127.0.0.1", cookie="ar_service")
    @real_node = real_node
    @sname = sname
    @cookie = cookie
  end

  def ar_object_to_erlang(object)
   if object.nil?
     return Erlix::Atom.new("undefined")
   end

   propslist=[]
    object.attributes.each do |column, value|
      ## add something here to make recursive call if 
      ## value is another active record object
      propslist.append(Erlix::Tuple.new([Erlix::List.new(column), value]))
    end
    Erlix::List.new(propslist)
  end

  def ar_find_by(tuple)
    unless tuple.match("{_, _, _}")
      return Erlix::Tuple.new([:error, "Wrong number of arguments dawg!"])
    end

    table = tuple.mget("{Table, _, _}", "Table")
    column = tuple.mget("{_, Column, _}", "Column")
    value = tuple.mget("{_, _, Value}", "Value")
    # Do a small sleep here because
    # activerecord (at least with sqlite)
    # can't seem to handle them if they come in
    # in rapid succession
    sleep 0.1
    ar_object_to_erlang(Kernel.const_get(table.to_s.capitalize).where(column.to_s.to_sym => value.data.to_s).first)
  end

  def handle_message(message)
    begin
      case
      when message.message.match("{find_by, Tuple}")
        args = message.message.mget("{find_by, Tuple}","Tuple")
        ar_find_by(args)
      when message.message.match("{where, Tuple}")
        Erlix::Tuple.new([:ok, :where])
      when message.message.match("{insert, Tuple}")
        Erlix::Tuple.new([:ok, :insert])
      when message.message.match("{update, Tuple}")
        Erlix::Tuple.new([:ok, :update])
      else
        Erlix::Tuple.new([:error, "I don't understand that."])
      end
    rescue Exception => e
      Erlix::Tuple.new([:error, e.message])
    end
  end

  def start(setup_fun=nil)
    unless @real_node
      configure
    end

    Erlix::Node.init(@sname, @cookie)
    
    c=Erlix::Connection.new(@real_node)
    puts "connect ok"
    
    process_id = Erlix::Pid.new(c)
    
    # make a Erlix::Tuple {Pid,test_atom} and send it to the real erlang-node
    c.esend(
             "my_pid",
             Erlix::Tuple.new(
                               [
                                 process_id,
                                 Erlix::Atom.new("session_started")
                               ]
                             )
           )
    puts "send ok"
    
    # start a new thread to receive the msg from the real erlang-node
    puts "receiving"
    t=Thread.new{
        if setup_fun
          setup_fun.call
        end
        while true do
            m=c.erecv
            result = handle_message(m)
            c.esend("my_pid",Erlix::Tuple.new([process_id, result]))
       end
    }
    
    t.join
  end
end
