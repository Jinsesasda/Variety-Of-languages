#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------

def parse(file)
  puts "Not yet implemented"    
end

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

def read_and_print_simple_file(file)
  line = file.gets
  if line == nil then return end
  
  # read 1st line, must be maze header
  sz, sx, sy, ex, ey = line.split(/\s/)
  puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"

  # read additional lines
  while line = file.gets do

    # begins with "path", must be path specification
    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      puts "path spec: #{name} starts at (#{x},#{y}) with dirs #{ds}"

    # otherwise must be cell specification (since maze spec must be valid)
    else
      x, y, ds, w = line.split(/\s/,4)
      puts "cell spec: coordinates (#{x},#{y}) with dirs #{ds}"
      ws = w.split(/\s/)
      ws.each {|w| puts "  weight #{w}"}
    end
  end
end

#----------------------------------
def main(command, fileName)
  maze_file = open(fileName)
   
   
  # perform command
  case command
  when "open"
    count = 0
    lines = maze_file.gets
    if lines == nil then return end
      while lines = maze_file.gets do
        a , b , c , d = lines.split(/\s/)
         if c =~ /[a-u]{4}/
          count = count +1
         end
      end
      return count
######################################################################################      
      
  when "bridge"  
   
    res = {}
     count = 0
     lines = maze_file.gets
     countArray = []
     a,b,c,d,e = lines.split(/\s/)
     size = a.to_i
    
      while lines = maze_file.gets do
        
      
        f,g,h,i,e,a,b = lines.split(/\s/)
        if f != "path"

          result = {} 
          array =[]
          
          if f !=nil
          array.push(f.to_i)
          end

          if g !=nil
          array.push(g.to_i)
          end

          array1 = []
           
          if h != nil
           
          array1.push(h)
          end
          

        
         
          res[array] = array1
           
           
         
                



            end 
          end
           count = 0
           
          for i in 0...size
            for j in 0...size
              xboundary1 = i
              yboundary1 = j
              

              dir = res[[i,j]].to_s
              
                 
              
              if dir.include?"d"
                 
                 yboundary1 = yboundary1 +1
                 
                 if yboundary1 <size
                  if res[[i,yboundary1]].to_s.include?"d"
                  
                    count = count +1
                  end
                end


              end
             

              if dir.include?"r"
                xboundary1 = xboundary1 +1
                if yboundary1 <size
                  if res[[xboundary1,j]].to_s.include?"r"
                   
                    count = count +1
                  end
                end
              end

              

            end
          end 

           return count
          
    
############################################################################################


  when "sortcells"
    zero = "0"
    one ="1"
    two = "2"
    three = "3"
    four = "4"


    lines = maze_file.gets
    if lines == nil then return end
      
      while lines = maze_file.gets do
      
        f,g,h,i = lines.split(/\s/)
         
        if f =~ /\d{1}/
          
        number = 0
         if(h != nil)
         number = h.length
         end
         if lines =~ /[a-u]{4}/
          four.concat(",(#{f},#{g})")
         
          
        elsif number == 3
          three.concat(",(#{f},#{g})")

        elsif number == 1
          one.concat(",(#{f},#{g})")

        elsif number == 2
          two.concat(",(#{f},#{g})")

        else
          zero.concat(",(#{f},#{g})")
         
        
         end
      end
    end
     
      return [zero, one, two, three ,four]
   
  when "paths"
     res = {}
     count = 0
     lines = maze_file.gets
     countArray = []

     pathCheck = false
      while lines = maze_file.gets do
        
      
        f,g,h,i,e,a,b = lines.split(/\s/)
        if f != "path"

          result = {} 
          array =[]
          
          if f !=nil
          array.push(f.to_i)
          end

          if g !=nil
          array.push(g.to_i)
          end

          array1 = []
           
          if h != nil
           
          array1.push(h)
          end
          if i != nil
          array1.push(i.to_f)
          end

          if e != nil
          array1.push(e.to_f)
          end

          if a != nil
            array1.push(a.to_f)
            end

            if b != nil
              array1.push(b.to_f)
              end  
            

          
          res[array] = array1
          
            end 
            
       
          if f =~ /path/ 
            if f!= nil && g!=nil && h!=nil && i!=nil &&e!=nil 
        count = 0
        xy = []  
        xy.push(h.to_i)
        xy.push(i.to_i)
        direction = []
        validDirection = []
                  
        
        for i in 0...e.length
          direction.push(e[i])
         

        end
           
        for i in 0...direction.length
          dir = direction[i]
          tempArr = res[xy]
          realArr = []
          
           validDir = []
          temp = tempArr[0]
           if temp.include? dir
              validDir.push(temp.include? dir)
         
          for j in 0...temp.length
            realArr.push(temp[j])
           
  
          end

           arrSize = 0
              
           if dir == "u"
            for k in 0...realArr.length
              if realArr[k] == dir
                weight = 1 + arrSize
                count = count + tempArr[weight]
              end
              arrSize = arrSize +1
            
              
            end
          
            xy[1] = xy[1] -1
             
           elsif dir == "r"
            for k in 0...realArr.length
              if realArr[k] == dir 
                weight = 1 + arrSize
                count = count + tempArr[weight]
              end
              arrSize = arrSize +1
            end
            xy[0] = xy[0] + 1

           elsif dir == "l"
            for k in 0...realArr.length
              if realArr[k] == dir
                weight = 1 + arrSize
                count = count + tempArr[weight]
              end
              arrSize = arrSize +1
            end
           xy[0] = xy[0] -1

           elsif dir =="d"
            for k in 0...realArr.length
              if realArr[k] == dir
                weight = 1 + arrSize
                count = count + tempArr[weight]
              end
              arrSize = arrSize +1
            end
            xy[1] = xy[1] +1
           end
           
           
         
          else
          
         validDirection.push(false)
           
        end   
       

      end
          
      if validDirection.include? false
        
      else
        result[count] = g 
        countArray.push(count)

         
  
      end
      
    end
    
      
          
      
   
  end
    end
    
    if countArray.empty?
     return "none"
    else
    countArray.sort!
    
    
    returnArr = []
    for m in 0...countArray.length
    
      
       whichPath ="%10.4f" % result.key(result[countArray[m]])
    
    
      finalWeight = result[countArray[m]] 
      
      finalString = "#{whichPath} #{finalWeight}"
      returnArr.push(finalString)
      
    end
  end
       return returnArr
      
    
      


when "distance"
  res = {}
  count = 0
  startOption = []
  lines = maze_file.gets
  countArray = []
  coord = {}

  

  f,g,h,i,e,a,b = lines.split(/\s/)
    startOption.push(f)
    startOption.push(g)
    startOption.push(h)
    startOption.push(i)
    startOption.push(e)
    

   while lines = maze_file.gets do
     
   
     f,g,h,i,e,a,b = lines.split(/\s/)
     if f != "path"

       array =[]
       if f !=nil && g !=nil
        if h != nil
        coord[[f.to_i, g.to_i]] = h
        else
          coord[[f.to_i, g.to_i]] =""
        end
      end

       if f !=nil
       array.push(f.to_i)
       end

       if g !=nil
       array.push(g.to_i)
       end

       array1 = []
        
       if h != nil
        
       array1.push(h)
       
       end
       
   
       

      
       res[array] = array1
         end 
        end
  x = startOption[1].to_i
  y = startOption[2].to_i

        

  result = {}
  hash = {}
  q = []
  q.push([x, y, 0])
  counter = 0
  hash[[x, y]] = counter
  while q != nil

    val = q.shift
  
    if val == nil 
      break
    end

    coord.each{|key, value| 
      if key == [val[0], val[1]]
        
        
        if value.include?("l") && !hash.key?([key[0]-1, key[1]])
          q.push([key[0]-1, key[1], val[2]+1])
          hash[[key[0]-1, key[1]]] = val[2]+1
        end
        if value.include?("u") && !hash.key?([key[0], key[1]-1])
          q.push([key[0], key[1]-1, val[2]+1])
          hash[[key[0], key[1]-1]] = val[2]+1
        end
        if value.include?("d") && !hash.key?([key[0], key[1]+1])
          q.push([key[0], key[1]+1, val[2]+1])
          hash[[key[0], key[1]+1]] = val[2]+1 
        end
        if value.include?("r") && !hash.key?([key[0]+1, key[1]])
          q.push([key[0]+1, key[1], val[2]+1])
          hash[[key[0]+1, key[1]]] = val[2]+1
        end
      end

    }   
  end

   returnArr = []
   sizeOfArr = startOption[0]

   




  
  temp = {}
  hash.each_pair{|key, value|
  if temp[value] == nil
    temp[value] = [key]
  else
    temp[value].append(key)
  end
}

for k, v in temp
  temp[k]=v.sort_by{|e| [e[0], e[1]]}
  
end
hash = {}
for k,v in temp
  hash[v] = k
end


hash.each{|key,value| value
      
result[value] = []
}

moves = 0


for i in 0..result.size-1
  resultStr = ""
  resultStr = "#{moves}" 
for k, v in hash
 
  if v == moves
    
  
    for i in k
    resultStr.concat(",(#{i})")
    end
    
  
  end
  
end
returnArr.push(resultStr)
  moves += 1

end

for i in 0..returnArr.size-1

returnArr[i].delete! "["
returnArr[i].delete! "]"
returnArr[i].delete! " "

end



 



    str = ""
     for i in returnArr
       str = str + i + "\n"
     end
     str = str[0...str.length-1]
   return str

 


  
 
    
       
   when "parse"
     parse(maze_file)
  

   when "print"
    startOption = []
  lines = maze_file.gets
  f,g,h,i,e,a,b = lines.split(/\s/)
    startOption.push(f.to_i)
    startOption.push(g.to_i)
    startOption.push(h.to_i)
    startOption.push(i.to_i)
    startOption.push(e.to_i)
     
    res = {}
    path = {}
    count = 0
    lines = maze_file.gets
    countArray = []

    pathCheck = false
     while lines = maze_file.gets do
       
        
       f,g,h,i,j = lines.split(/\s/)
       if f != "path"

         result = {} 
         array =[]
         
         if f !=nil
         array.push(f.to_i)
         end

         if g !=nil
         array.push(g.to_i)
         end

         array1 = []
          
         if h != nil
          
         array1.push(h.to_s)
         end

         
        else
        
          array3 =[]
           
          if g != nil
            array3.push(g)
          end

          if h !=nil
          array3.push(h)
          end
 
          if i !=nil
          array3.push(i)
          end
 
          array4 = []
           
          if j != nil
            array4.push(j)



          end  
          if path[array3] == nil
            path[array3] = array4
          else
            path[array3].push(array4[0])
          end
        end
           
         
         res[array] = array1
           end 
           

          
          

         
########################################################################

           size = startOption[0]*2 + 1
           s = [startOption[1], startOption[2]]
          e = [startOption[3], startOption[4]]

          grid = Array.new(size){Array.new(size)}
          grid[2*s[1]+ 1][2*s[0]+1] = 's'
          grid[2*e[1]+1][2*e[0]+1] = 'e'

          for i in 0...size
            for j in 0...size
              if i % 2 == 0
                if j %2 == 0
                  grid[i][j] = '+'
                elsif i == 0 || i == size-1
                    grid[i][j] = '-'
                end
              end

              if j % 2 == 0
                if i %2 == 0
                  grid[i][j] = '+'
                elsif j == 0 || j == size-1
                    grid[i][j] = '|'
                end
              end

              if grid[i][j] == nil
                grid[i][j] = " "
              end
            end
          end

          for i in 0...startOption[0]
            for j in 0...startOption[0]
              if res[[i, j]] != [] && res[[i, j]] != nil
                val = res[[i, j]][0]
              elsif res[[i, j]] == nil
                val = "nil"
              else
                val = "nonexistent"
              end

              if val == "nonexistent"
                grid[2*j+1][2*i+1-1] = "|"
                grid[2*j+1][2*i+1+1] = "|"
                grid[2*j+1-1][2*i+1] = "-"
                grid[2*j+1+1][2*i+1] = "-"
              else
                if (i== 0 and j == 0) ||(i==startOption[0]-1 and j == startOption[0]-1)
                  next
                end               
                if !val.include?("u")
                  grid[2*j+1-1][2*i+1] = "-"
                end
                if !val.include?("d")
                  grid[2*j+1+1][2*i+1] = "-"
                end
                if !val.include?("l")
                  grid[2*j+1][2*i+1-1] = "|"
                end
                if !val.include?("r")
                  grid[2*j+1][2*i+1+1] = "|"
                end
              end
            end
          end

###########################################################################################          
           
          str = fileName
          s = shortDir(fileName)
          if (s !="none")
          arr = []
          for i in s
            arr.append(i.split(" "))
          end
          start = []
          short = arr[0][1]

          arr = path.keys

          for i in arr
              if i[0] == short
                start = [i[1].to_i, i[2].to_i]
                break
              end
          end

          startfound = false
          endfound = false
          i, j = start[0], start[1]
          if grid[2*j+1][2*i+1] != 's'
            grid[2*j+1][2*i+1] = '*'
          else
            grid[2*j+1][2*i+1] = "S"
            startfound = true
          end
          pathDir = path[[short, i.to_s, j.to_s]][0]
          
          pathDir = pathDir.split("").reverse!
          pathLength = pathDir.length - 1

          def endcheck(grid, l2, l1)
            if grid[l2][l1].downcase == "e"
              return true
            else
              return false
            end
            end
            def startcheck(grid, l2, l1)
              if grid[l2][l1].downcase == "s"
                return true
              else
                return false
              end
              end
          # # grid[2*pathArr[0]-1][2*pathArr[1]+1] = "*"
          
          while pathDir.length != 0
            dir = pathDir.pop
            l1, l2 = 2*i+1, 2*j+1
            
            if dir == "u"
              if endcheck(grid, l2-2, l1)
              endfound = true
                if endfound && startfound
                  grid[l2-2][l1] = "E"
                end
                break
              elsif startcheck(grid, l2-2, l1)
                startfound=true
                # if endfound && startfound
                grid[l2-2][l1] = "S"
                # end
                break

              end
              grid[l2-2][l1] = "*"
              j -= 1
            elsif dir == "d"
              if endcheck(grid, l2+2, l1)
                endfound = true
                if endfound && startfound
                grid[l2+2][l1] = "E"
                end
                break
              elsif startcheck(grid, l2+2, l1)
                startfound=true
                # if endfound && startfound
                grid[l2+2][l1] = "S"
                # end
                break

              end
              grid[l2+2][l1] = "*"
              j +=1
            elsif dir == "l"
              if endcheck(grid, l2, l1-2)
                endfound = true
                if endfound && startfound
                grid[l2][l1-2] = "E"
                end
                break
              elsif startcheck(grid, l2, l1-2)
                startfound=true
                # if endfound && startfound
                grid[l2][l1-2] = "S"
                # end
                break
              end
              grid[l2][l1-2] = "*"
              i -=1
            else
              if endcheck(grid, l2, l1+2)
                endfound = true
                if endfound && startfound
                grid[l2][l1+2] = "E"
                end
                break
              elsif startcheck(grid, l2, l1+2)
                startfound=true
                # if endfound && startfound
                grid[l2][l1+2] = "S"
                # end
                break
              end
              grid[l2][l1+2] = "*"
              i +=1
            end
          end
        

        end


          # for i in 0...pathLength
        
          #   if pathDir.include? "u"
              
          #     grid[2*pathArr[0]-1][2*pathArr[1]+1] = "*"
          #    pathArr[0] = pathArr[0] -1
          #   end

          #   if pathDir.include? "d"
          #     grid[2*pathArr[0]-1][2*pathArr[1]+1] = "*"
          #     pathArr[0] = pathArr[0] +1
          #   end
          #   if pathDir.include? "r"
          #     grid[2*pathArr[0]+1][2*pathArr[1]+1] = "*"
          #     pathArr[1] = pathArr[1] +1
          #   end
          #   if pathDir.include? "l"
          #     grid[2*pathArr[0]+1][2*pathArr[1]-1] = "*"
          #     pathArr[1] = pathArr[1] -1
          #   end
          # end
      
 ####################################################################################           


          
      
      

         

            str = ""
          for i in 0...grid.length
            str = str + grid[i].join + "\n"
          end
          
          return str[0...str.length-1]

       
##############################################################################







        when "solve"
          res = {}
          count = 0
          startOption = []
          lines = maze_file.gets
          countArray = []
          coord = {}
        
          
        
          f,g,h,i,e,a,b = lines.split(/\s/)
            startOption.push(f)
            startOption.push(g)
            startOption.push(h)
            startOption.push(i)
            startOption.push(e)
            
        
           while lines = maze_file.gets do
             
           
             f,g,h,i,e,a,b = lines.split(/\s/)
             if f != "path"
        
               array =[]
               if f !=nil && g !=nil
                if h != nil
                coord[[f.to_i, g.to_i]] = h
                else
                  coord[[f.to_i, g.to_i]] =""
                end
              end
        
               if f !=nil
               array.push(f.to_i)
               end
        
               if g !=nil
               array.push(g.to_i)
               end
        
               array1 = []
                
               if h != nil
                
               array1.push(h)
               
               end
               
           
               
        
              
               res[array] = array1
                 end 
                end
          x = startOption[1].to_i
          y = startOption[2].to_i
        
                
        
          result = {}
          hash = {}
          q = []
          q.push([x, y, 0])
          counter = 0
          hash[[x, y]] = counter
          while q != nil
        
            val = q.shift
          
            if val == nil 
              break
            end
        
            coord.each{|key, value| 
              if key == [val[0], val[1]]
                if value.include?("u") && !hash.key?([key[0], key[1]-1])
                  q.push([key[0], key[1]-1, val[2]+1])
                  hash[[key[0], key[1]-1]] = val[2]+1
                end
                if value.include?("d") && !hash.key?([key[0], key[1]+1])
                  q.push([key[0], key[1]+1, val[2]+1])
                  hash[[key[0], key[1]+1]] = val[2]+1 
                end
                if value.include?("l") && !hash.key?([key[0]-1, key[1]])
                  q.push([key[0]-1, key[1], val[2]+1])
                  hash[[key[0]-1, key[1]]] = val[2]+1
                end
                if value.include?("r") && !hash.key?([key[0]+1, key[1]])
                  q.push([key[0]+1, key[1], val[2]+1])
                  hash[[key[0]+1, key[1]]] = val[2]+1
                end
              end
        
            }   
          end
        
         returnVal = true
         
        
         if hash.key?([startOption[3].to_i,startOption[4].to_i])
          returnVal = true
         else
          returnVal = false
       end

         return returnVal

           
    
         
        
        
          
        
        
   



  

      
         
  end
end 




def shortDir(f)
  maze_file = open(f)
 
res = {}
count = 0
lines = maze_file.gets
countArray = []

pathCheck = false
 while lines = maze_file.gets do
   
 
   f,g,h,i,e,a,b = lines.split(/\s/)
   if f != "path"

     result = {} 
     array =[]
     
     if f !=nil
     array.push(f.to_i)
     end

     if g !=nil
     array.push(g.to_i)
     end

     array1 = []
      
     if h != nil
      
     array1.push(h)
     end
     if i != nil
     array1.push(i.to_f)
     end

     if e != nil
     array1.push(e.to_f)
     end

     if a != nil
       array1.push(a.to_f)
       end

       if b != nil
         array1.push(b.to_f)
         end  
       

     
     res[array] = array1
     
       end 
       
  
     if f =~ /path/ 
       if f!= nil && g!=nil && h!=nil && i!=nil &&e!=nil 
   count = 0
   xy = []  
   xy.push(h.to_i)
   xy.push(i.to_i)
   direction = []
   validDirection = []
             
   
   for i in 0...e.length
     direction.push(e[i])
    

   end
      
   for i in 0...direction.length
     dir = direction[i]
     tempArr = res[xy]
     realArr = []
     
      validDir = []
     temp = tempArr[0]
      if temp.include? dir
         validDir.push(temp.include? dir)
    
     for j in 0...temp.length
       realArr.push(temp[j])
      

     end

      arrSize = 0
         
      if dir == "u"
       for k in 0...realArr.length
         if realArr[k] == dir
           weight = 1 + arrSize
           count = count + tempArr[weight]
         end
         arrSize = arrSize +1
       
         
       end
     
       xy[1] = xy[1] -1
        
      elsif dir == "r"
       for k in 0...realArr.length
         if realArr[k] == dir 
           weight = 1 + arrSize
           count = count + tempArr[weight]
         end
         arrSize = arrSize +1
       end
       xy[0] = xy[0] + 1

      elsif dir == "l"
       for k in 0...realArr.length
         if realArr[k] == dir
           weight = 1 + arrSize
           count = count + tempArr[weight]
         end
         arrSize = arrSize +1
       end
      xy[0] = xy[0] -1

      elsif dir =="d"
       for k in 0...realArr.length
         if realArr[k] == dir
           weight = 1 + arrSize
           count = count + tempArr[weight]
         end
         arrSize = arrSize +1
       end
       xy[1] = xy[1] +1
      end   
     else   
    validDirection.push(false)
      
   end   
  

 end
     
 if validDirection.include? false
   
 else
   result[count] = g 
   countArray.push(count)
 end
end
end
end

if countArray.empty?
return "none"
else
countArray.sort!


returnArr = []
for m in 0...countArray.length

 
  whichPath ="%10.4f" % result.key(result[countArray[m]])


 finalWeight = result[countArray[m]] 
 
 finalString = "#{whichPath} #{finalWeight}"
 returnArr.push(finalString)
 
end
end

  return returnArr
end