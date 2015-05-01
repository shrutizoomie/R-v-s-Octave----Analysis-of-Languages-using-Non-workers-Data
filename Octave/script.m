clear;
disp("a.Overall Analysis.a1.Analysis based Rural/Urban on Grand Total b.Analysis based on Gender b1.Analysis based actvity of the non-workers of each Gender c.Analysis based on Disability d.Analysis based on Rural Area e.Analysis based on Urban Area z.summary and exit\n"); 
choice= input('Enter your choice: ','s');
area_name= input('Enter Area Name: ','s');
area_name = tolower(area_name);
vflag = 0;
pos = 0;
pos1 = 0;

s2 = "india";
if (strcmp (area_name, s2) == 1)
    vflag=1;
else
    vflag=0;
endif


#exit(0);
dflist = {"JK", "HP", "PUNJAB", "CHANDIGARH", "UTTARAKHAND","HARYANA", "DELHI", "RAJASTHAN", "UP", "BIHAR", "SIKKIM", "ARUNACHAL", "NAGALAND","MANIPUR", "MIZORAM", "TRIPURA", "MEGHALAYA", "ASSAM", "WB", "JHARKHAND", "ODISHA", "CHHATTISGARH", "MP", "GUJARAT", "DIU", "DADRA", "MAHARASHTRA", "ANDHRA", "KARNATAKA", "GOA","LAKSHADWEEP", "KERALA", "TN", "PUDUCHERRY","ANDAMAN"};
     
area_name = toupper(area_name);
if (vflag == 0)
    for i = 1:35
       if (strcmp (area_name, dflist{i}) == 1)
       	vflag = 1;
       	pos = i;
       	pos1 = i;
       	break;
       end
    end
end
if (vflag == 0)
   disp("Invalid area name, please execute again\n");
   disp("Area Name should be any one displayed\n");
   disp("INDIA ");
   for i = 1:35
      disp(dflist{i});
      disp(" ");
   end
endif

if(vflag ==1)
  fid = fopen('Disabled-Non-Workers.csv', 'r');
  Non_Workers = textscan(fid,'%s%d%d%s%s%s%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d','delimiter',',');
  if (strcmp (choice, "a") == 1)
      pos = pos*24 + 2;
      types = [Non_Workers{7}];
      types_data = [types(pos,1), types(pos+1,1), types(pos+2,1), types(pos+3,1), types(pos+4,1), types(pos+5,1), types(pos+6,1)];
      h = bar (types_data,w=0.9);
      h=get (gcf, "currentaxes");
      set(h,"fontweight","bold");
      set(h,"xtick",[1 2 3 4 5 6 7]);
      set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
      title (cstrcat("Activity of Non-Worker in", " ", area_name));
      #text(types(pos,1), types(pos+1,1), types(pos+2,1), types(pos+3,1), types(pos+4,1), types(pos+5,1), types(pos+6,1));
      set(h,"interpreter","tex");
  elseif (strcmp (choice, "a1") == 1)
      pos = pos*24 + 10;
      types = [Non_Workers{7}];
      types_data = [types(pos,1),types(pos+8,1); types(pos+1,1),types(pos+9,1); types(pos+2,1),types(pos+10,1); types(pos+3,1),types(pos+11,1); types(pos+4,1),types(pos+12,1); types(pos+5,1),types(pos+13,1); types(pos+6,1),types(pos+14,1)];
      h = bar (types_data,w=0.9);
      h=get (gcf, "currentaxes");
      set(h,"fontweight","bold");
      set(h,"xtick",[1 2 3 4 5 6 7]);
      set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
      title (cstrcat("Activity of Non-Worker in", " ", area_name));
      legend ('Rural', 'Urban');
      legend right;
      set(h,"interpreter","tex");    
  #elseif (strcmp (choice, "a2") == 1)
  #    pos = pos*24 + 2;
  #    pos
  #    types = [Non_Workers{7}];
  #    sum = 0.00;
  #    types_data = [types(pos,1), types(pos+1,1), types(pos+2,1), types(pos+3,1), types(pos+4,1), types(pos+5,1), types(pos+6,1)];
  #    types_data
  #    for entry = types_data,
  #       sum = sum + entry;
  #    end;
  #
  #   types_data_per =[types(pos,1)/sum*100, types(pos+1,1)/sum*100, types(pos+2,1)/sum*100,types(pos+3,1)/sum*100, types(pos+4,1)/sum*100, types_data(5,1)/sum*100, types_data(6,1)/sum*100];
  #   types_data_per
  #     pie ([types(pos,1), types(pos+1,1), types(pos+2,1)], types(pos+3,1), types(pos+4,1), types(pos+5,1), types(pos+6,1)], [0, 0, 0, 0, 0, 0, 1], {'Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others'});
  #     title('Pie Chart of Six Larges Oil Nations');  

xlabel('Billions of Barrels of Oil');
  elseif (strcmp (choice, "b") == 1) 
      pos = pos*24 + 2;
      types = [Non_Workers{8}, Non_Workers{9}];
      types_data1 = [types(pos,1) types(pos,2); types(pos+1,1) types(pos+1,2); types(pos+2,1) types(pos+2,2); types(pos+3,1) types(pos+3,2); types(pos+4,1) types(pos+4,2); types(pos+5,1) types(pos+5,2); types(pos+6,1) types(pos+6,2)];
      h=bar(types_data1, 'stacked');
      h=get (gcf, "currentaxes");
      set(h,"fontweight","bold");
      set(h,"xtick",[1 2 3 4 5 6 7]);
      set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
      title (cstrcat("Disabled Non-Workers Based on Gender in", " ", area_name));
      legend('Male', 'Female') 
   elseif (strcmp (choice, "b1") == 1) 
      pos = pos*24 + 10;
      pos1 = pos1*24 + 18;
      types = [Non_Workers{8}, Non_Workers{9}];
      types_data1 = [types(pos,1) types(pos,2) types(pos1,1) types(pos1,2);
      		     types(pos+1,1) types(pos+1,2) types(pos1+1,1) types(pos1+1,2);
      		     types(pos+2,1) types(pos+2,2) types(pos1+2,1) types(pos1+2,2);
      		     types(pos+3,1) types(pos+3,2) types(pos1+3,1) types(pos1+3,2);
      		     types(pos+4,1) types(pos+4,2) types(pos1+4,1) types(pos1+4,2);
      		     types(pos+5,1) types(pos+5,2) types(pos1+5,1) types(pos1+5,2);
      		     types(pos+6,1) types(pos+6,2) types(pos1+6,1) types(pos1+6,2);];      
      h=bar(types_data1,w=0.9);
      h=get (gcf, "currentaxes");
      set(h,"fontweight","bold");
      set(h,"xtick",[1 2 3 4 5 6 7]);
      set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
      title (cstrcat("Activity of Non-Worker based on Gender in Rural/Urban of", " ", area_name));
      legend('Male-Rural', 'Female-Rural','Male-Urban', 'Female-Urban');
      legend right;
      set(h,"interpreter","tex"); 
  elseif (strcmp (choice, "c") == 1) 
      	pos = pos*24 + 2;
      	types = [Non_Workers{10}, Non_Workers{13}, Non_Workers{16}, Non_Workers{19}, Non_Workers{22}, Non_Workers{25}, Non_Workers{28}, Non_Workers{31}];
      	types_data1 = [types(pos,1) types(pos,2) types(pos,3) types(pos,4) types(pos,5) types(pos,6) types(pos,7) types(pos,8); types(pos+1,1) types(pos+1,2) types(pos+1,3) types(pos+1,4) types(pos+1,5) types(pos+1,6) types(pos+1,7) types(pos+1,8); types(pos+2,1) types(pos+2,2) types(pos+2,3) types(pos+2,4) types(pos+2,5) types(pos+2,6) types(pos+2,7) types(pos+2,8); types(pos+3,1) types(pos+3,2) types(pos+3,3) types(pos+3,4) types(pos+3,5) types(pos+3,6) types(pos+3,7) types(pos+3,8); types(pos+4,1) types(pos+4,2) types(pos+4,3) types(pos+4,4) types(pos+4,5) types(pos+4,6) types(pos+4,7) types(pos+4,8); types(pos+5,1) types(pos+5,2) types(pos+5,3) types(pos+5,4) types(pos+5,5) types(pos+5,6) types(pos+5,7) types(pos+5,8); types(pos+6,1) types(pos+6,2) types(pos+6,3) types(pos+6,4) types(pos+6,5) types(pos+6,6) types(pos+6,7) types(pos+6,8)];
        h=bar(types_data1, w=0.9);
        h=get (gcf, "currentaxes");
        set(h,"fontweight","bold");
        set(h,"xtick",[1 2 3 4 5 6 7]);
        set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
        title (cstrcat("Activity of Non-Worker based on Disability in", " ", area_name));
        legend('Seeing', 'Hearing', 'Speech', 'Movement', 'Mental retardation', 'Mental illness', 'Any other', 'Multiple disability')
   elseif (strcmp (choice, "d") == 1) 
     	pos = pos*24 + 10;
        types = [Non_Workers{10}, Non_Workers{13}, Non_Workers{16}, Non_Workers{19}, Non_Workers{22}, Non_Workers{25}, Non_Workers{28}, Non_Workers{31}];
      	types_data1 = [types(pos,1) types(pos,2) types(pos,3) types(pos,4) types(pos,5) types(pos,6) types(pos,7) types(pos,8); types(pos+1,1) types(pos+1,2) types(pos+1,3) types(pos+1,4) types(pos+1,5) types(pos+1,6) types(pos+1,7) types(pos+1,8); types(pos+2,1) types(pos+2,2) types(pos+2,3) types(pos+2,4) types(pos+2,5) types(pos+2,6) types(pos+2,7) types(pos+2,8); types(pos+3,1) types(pos+3,2) types(pos+3,3) types(pos+3,4) types(pos+3,5) types(pos+3,6) types(pos+3,7) types(pos+3,8); types(pos+4,1) types(pos+4,2) types(pos+4,3) types(pos+4,4) types(pos+4,5) types(pos+4,6) types(pos+4,7) types(pos+4,8); types(pos+5,1) types(pos+5,2) types(pos+5,3) types(pos+5,4) types(pos+5,5) types(pos+5,6) types(pos+5,7) types(pos+5,8); types(pos+6,1) types(pos+6,2) types(pos+6,3) types(pos+6,4) types(pos+6,5) types(pos+6,6) types(pos+6,7) types(pos+6,8)];
        h=bar(types_data1,"stacked");
        h=get (gcf, "currentaxes");
        set(h,"fontweight","bold");
        set(h,"xtick",[1 2 3 4 5 6 7]);
        set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
        title (cstrcat("Activity of Non-Worker based on Disability in Rural part of ", " ", area_name));
        legend('Seeing', 'Hearing', 'Speech', 'Movement', 'Mental retardation', 'Mental illness', 'Any other', 'Multiple disability')
      	  
   elseif (strcmp (choice, "e") == 1) 
      	pos = pos*24 + 18;
      	types = [Non_Workers{10}, Non_Workers{13}, Non_Workers{16}, Non_Workers{19}, Non_Workers{22}, Non_Workers{25}, Non_Workers{28}, Non_Workers{31}];
      	types_data1 = [types(pos,1) types(pos,2) types(pos,3) types(pos,4) types(pos,5) types(pos,6) types(pos,7) types(pos,8); types(pos+1,1) types(pos+1,2) types(pos+1,3) types(pos+1,4) types(pos+1,5) types(pos+1,6) types(pos+1,7) types(pos+1,8); types(pos+2,1) types(pos+2,2) types(pos+2,3) types(pos+2,4) types(pos+2,5) types(pos+2,6) types(pos+2,7) types(pos+2,8); types(pos+3,1) types(pos+3,2) types(pos+3,3) types(pos+3,4) types(pos+3,5) types(pos+3,6) types(pos+3,7) types(pos+3,8); types(pos+4,1) types(pos+4,2) types(pos+4,3) types(pos+4,4) types(pos+4,5) types(pos+4,6) types(pos+4,7) types(pos+4,8); types(pos+5,1) types(pos+5,2) types(pos+5,3) types(pos+5,4) types(pos+5,5) types(pos+5,6) types(pos+5,7) types(pos+5,8); types(pos+6,1) types(pos+6,2) types(pos+6,3) types(pos+6,4) types(pos+6,5) types(pos+6,6) types(pos+6,7) types(pos+6,8)];
        h=bar(types_data1,"stacked");
        h=get (gcf, "currentaxes");
        set(h,"fontweight","bold");
        set(h,"xtick",[1 2 3 4 5 6 7]);
        set(h,"xticklabel",['Student';'Household' ;'Dependent';'Pensioner';'Rentier' ;'Beggar';'Others']);
        title (cstrcat("Activity of Non-Worker based on Disability in Urban part of ", " ", area_name));
        legend('Seeing', 'Hearing', 'Speech', 'Movement', 'Mental retardation', 'Mental illness', 'Any other', 'Multiple disability');
   elseif (strcmp (choice, "z") == 1) 
   	pos = 25;
   	types = [Non_Workers{7}];
   	types_data = [types(pos,1), types(pos+24,1),types(pos+48,1), types(pos+72,1),types(pos+96,1), types(pos+120,1),types(pos+144,1), types(pos+168,1),types(pos+192,1), types(pos+216,1),types(pos+240,1), types(pos+264,1),types(pos+288,1), types(pos+312,1),types(pos+336,1), types(pos+360,1),types(pos+384,1), types(pos+408,1),types(pos+432,1), types(pos+456,1),types(pos+480,1), types(pos+504,1),types(pos+528,1), types(pos+552,1),types(pos+576,1), types(pos+600,1),types(pos+624,1), types(pos+648,1),types(pos+672,1), types(pos+696,1),types(pos+720,1), types(pos+744,1),types(pos+768,1), types(pos+792,1),types(pos+816,1)];
  	h=bar(types_data,w=0.5);
	h=get (gcf, "currentaxes");
	set(h,"fontweight","bold");
        set(h,"xtick",[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35]);
        set(h,"xticklabel",dflist);
        title ("Non Wokers due to Disability in each state in INDIA");
   else
   	disp("Invalid choice, please execute again");	

  endif

 endif
 
 
 
 
 
 