function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
  slides.instructions1 = slide({
    name : "instructions1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst1 = "";
//    	console.log(block_order);
    	if (exp.stims_block1[0].block == "ai") {
    		inst1 = inst1 + "First you'll answer questions about what the people at the party are asking about."
    	} else {
    		inst1 = inst1 + "First you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst1").html(inst1);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    start : function() {
      $(".err").hide();
    },
    present_handle : function(stim) {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	 
      console.log(this.stim);    
      var utterance = "<strong>"+this.stim.name + ":</strong> \"<i>"+this.stim.name2 + " " + this.stim.utterance+"</i>\""
      // var utterance = "<p>"+this.stim.name + ": \"<i>"+this.stim.utterance+"</i>\"</p>" +"<p>"+this.stim.name2 + ": \"<i>Are you sure?</i>\"</p>"+this.stim.name + ": \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  question = "Is "+this.stim.name+"'s utterance contradictory?";
	  // console.log(this.stim.block);
// 	  if (this.stim.block == "ai") {
// 	  		question = "Is "+this.stim.name+" asking whether "+this.stim.question+"?";
// 	  } else {
// 	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
// 	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block1",
      "question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  slides.instructions2 = slide({
    name : "instructions2",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
    	var inst2 = "That was the first half! ";
    	if (exp.stims_block2[0].block == "ai") {
    		inst2 = inst2 + "Now you'll answer questions about what the people at the party are asking about."
    	} else {
    		inst2 = inst2 + "Now you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst2").html(inst2);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });   
  
  slides.block2 = slide({
    name : "block2",
    present : exp.stims_block2,
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
      $(".err").hide();
    },
    present_handle : function(stim) {
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	      
      var utterance = this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);	  
	  if (this.stim.block == "ai") {
	  		question = "Is "+this.stim.name+" asking whether "+this.stim.question+"?";
	  } else {
	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider2", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block2",
      "question_type" : this.stim.block,     
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,   	  
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });        
 

  slides.questionaire =  slide({
    name : "questionaire",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
//        enjoyment : $("#enjoyment").val(),
//        asses : $('input[name="assess"]:checked').val(),
        american : $('input[name="ame"]:checked').val(),
        age : $("#age").val(),
//        gender : $("#gender").val(),
//        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.finished = slide({
    name : "finished",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  var speaker_names = _.shuffle([
    {
      "name":"James",
      "gender":"M"
    },
//    {
//      "name":"John",
//      "gender":"M"
//    },
    {
      "name":"Robert",
      "gender":"M"
    },
//     {
//       "name":"Michael",
//       "gender":"M"
//     },
    {
      "name":"William",
      "gender":"M"
    },
    {
      "name":"David",
      "gender":"M"
    },
//    {
//      "name":"Richard",
//      "gender":"M"
//    },
    {
      "name":"Joseph",
      "gender":"M"
    },
    {
      "name":"Charles",
      "gender":"M"
    },
    {
      "name":"Thomas",
      "gender":"M"
    },
    {
      "name":"Christopher",
      "gender":"M"
    },
    {
      "name":"Daniel",
      "gender":"M"
    },
    {
      "name":"Matthew",
      "gender":"M"
    },
//    {
//      "name":"Donald",
//      "gender":"M"
//    },
    {
      "name":"Anthony",
      "gender":"M"
    },
    {
      "name":"Paul",
      "gender":"M"
    },
//    {
//      "name":"Mark",
//      "gender":"M"
//    },
    {
      "name":"George",
      "gender":"M"
    },
    {
      "name":"Steven",
      "gender":"M"
    },
    {
      "name":"Kenneth",
      "gender":"M"
    },
    {
      "name":"Jennifer",
      "gender":"F"
    },
    {
      "name":"Elizabeth",
      "gender":"F"
    },
    {
      "name":"Linda",
      "gender":"F"
    },
    {
      "name":"Emily",
      "gender":"F"
    },
//    {
//      "name":"Susan",
//      "gender":"F"
//    },
//     {
//       "name":"Margaret",
//       "gender":"F"
//     },
    {
      "name":"Jessica",
      "gender":"F"
    },
    {
      "name":"Dorothy",
      "gender":"F"
    },
//     {
//       "name":"Sarah",
//       "gender":"F"
//     },
    {
      "name":"Karen",
      "gender":"F"
    },
    {
      "name":"Nancy",
      "gender":"F"
    },
//     {
//       "name":"Betty",
//       "gender":"F"
//     },
    {
      "name":"Lisa",
      "gender":"F"
    },
    {
      "name":"Sandra",
      "gender":"F"
    },
//     {
//       "name":"Helen",
//       "gender":"F"
//     },
    {
      "name":"Ashley",
      "gender":"F"
    },
    {
      "name":"Donna",
      "gender":"F"
    },
    {
      "name":"Kimberly",
      "gender":"F"
    },
    {
      "name":"Carol",
      "gender":"F"
    },
    {
      "name":"Michelle",
      "gender":"F"
    }]);


var subject_names = _.shuffle([
//    {
//      "name":"Andrew",
//      "gender":"M"
//    },
    {
      "name":"Edward",
      "gender":"M"
    },
    {
      "name":"Joshua",
      "gender":"M"
    },
    {
      "name":"Brian",
      "gender":"M"
    },
    {
      "name":"Kevin",
      "gender":"M"
    },
    {
      "name":"Ronald",
      "gender":"M"
    },
    {
      "name":"Timothy",
      "gender":"M"
    },
    {
      "name":"Jason",
      "gender":"M"
    },
    {
      "name":"Jeffrey",
      "gender":"M"
    },
    {
      "name":"Gary",
      "gender":"M"
    },
    {
      "name":"Ryan",
      "gender":"M"
    },
    {
      "name":"Nicholas",
      "gender":"M"
    },
    {
      "name":"Eric",
      "gender":"M"
    },
    {
      "name":"Jacob",
      "gender":"M"
    },
    {
      "name":"Jonathan",
      "gender":"M"
    },
    {
      "name":"Larry",
      "gender":"M"
    },
//    {
//      "name":"Frank",
//      "gender":"M"
//    },
    {
      "name":"Scott",
      "gender":"M"
    },
    {
      "name":"Justin",
      "gender":"M"
    },
    {
      "name":"Brandon",
      "gender":"M"
    },
    {
      "name":"Raymond",
      "gender":"M"
    },
    {
      "name":"Gregory",
      "gender":"M"
    },
    {
      "name":"Samuel",
      "gender":"M"
    },
    {
      "name":"Benjamin",
      "gender":"M"
    },
    {
      "name":"Patrick",
      "gender":"M"
    },
//    {
//      "name":"Jack",
//      "gender":"M"
//    },
    {
      "name":"Dennis",
      "gender":"M"
    },
    {
      "name":"Jerry",
      "gender":"M"
    },
    {
      "name":"Alexander",
      "gender":"M"
    },
    {
      "name":"Tyler",
      "gender":"M"
    },
//    {
//      "name":"Mary",
//      "gender":"F"
//    },
    {
      "name":"Emily",
      "gender":"F"
    },
//     {
//       "name":"Amanda",
//       "gender":"F"
//     },
    {
      "name":"Melissa",
      "gender":"F"
    },
    {
      "name":"Deborah",
      "gender":"F"
    },
    {
      "name":"Laura",
      "gender":"F"
    },
    {
      "name":"Stephanie",
      "gender":"F"
    },
    {
      "name":"Rebecca",
      "gender":"F"
    },
    {
      "name":"Sharon",
      "gender":"F"
    },
    {
      "name":"Cynthia",
      "gender":"F"
    },
    {
      "name":"Kathleen",
      "gender":"F"
    },
    {
      "name":"Ruth",
      "gender":"F"
    },
//    {
//      "name":"Anna",
//      "gender":"F"
//    },
    {
      "name":"Shirley",
      "gender":"F"
    },
    {
      "name":"Amy",
      "gender":"F"
    },
    {
      "name":"Angela",
      "gender":"F"
    },
    {
      "name":"Virginia",
      "gender":"F"
    },
    {
      "name":"Brenda",
      "gender":"F"
    },
 //    {
//       "name":"Catherine",
//       "gender":"F"
//     },
    {
      "name":"Nicole",
      "gender":"F"
    },
    {
      "name":"Christina",
      "gender":"F"
    },
//     {
//       "name":"Janet",
//       "gender":"F"
//     },
//     {
//       "name":"Samantha",
//       "gender":"F"
//     },
    {
      "name":"Carolyn",
      "gender":"F"
    },
    {
      "name":"Rachel",
      "gender":"F"
    },
    {
      "name":"Heather",
      "gender":"F"
    },
    {
      "name":"Diane",
      "gender":"F"
    },
//     {
//       "name":"Joyce",
//       "gender":"F"
//     },
    {
      "name":"Julie",
      "gender":"F"
    },
    {
      "name":"Emma",
      "gender":"F"
    }
  ]);

var items = _.shuffle([ 
   {
     "trigger":"annoyed",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"know",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"discover",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"reveal",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"see",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"establish",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"pretend",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"think",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"suggest",
     "trigger_class":"C"
   }, 
   {
     "trigger":"prove",
     "trigger_class":"C"
   }, 
   {
     "trigger":"demonstrate",
     "trigger_class":"C"
   }, 
   {
     "trigger":"say",
     "trigger_class":"C"
   },
   {
     "trigger":"hear",
     "trigger_class":"C"
   },
   {
     "trigger":"confess",
     "trigger_class":"C"
   }, 
   {
     "trigger":"inform_Sam",
     "trigger_class":"C"
   }, 
   {
     "trigger":"announce",
     "trigger_class":"C"
   }, 
   {
     "trigger":"acknowledge",
     "trigger_class":"C"
   },
   {
     "trigger":"admit",
     "trigger_class":"C"
   },
   {
     "trigger":"confirm",
     "trigger_class":"C"
   },
   {
     "trigger":"be_right_that",
     "trigger_class":"C"
   }
 ]);

 var contents = {
 "1": {
  "annoyed":"is annoyed that Mary is pregnant, but I know that Mary isn't.",
  "know":"knows that Mary is pregnant, but I know that Mary isn't.",
  "discover":"discovered that Mary is pregnant, but I know that Mary isn't.",
  "reveal":"revealed that Mary is pregnant, but I know that Mary isn't.",
  "see" :"saw that Mary is pregnant, but I know that Mary isn't.",
  "establish":"established that Mary is pregnant, but I know that Mary isn't.",
  "pretend":"pretended that Mary is pregnant, but I know that Mary isn't.",
  "think":"thinks that Mary is pregnant, but I know that Mary isn't.",
  "suggest":"suggested that Mary is pregnant, but I know that Mary isn't.",
  "prove":"proved that Mary is pregnant, but I know that Mary isn't.",
  "demonstrate":"demonstrated that Mary is pregnant, but I know that Mary isn't.",
  "say":"said that Mary is pregnant, but I know that Mary isn't.",
  "hear":"heard that Mary is pregnant, but I know that Mary isn't.",
  "confess":"confessed that Mary is pregnant, but I know that Mary isn't.",
  "inform_Sam":"informed Sam that Mary is pregnant, but I know that Mary isn't.",
  "announce":"announced that Mary is pregnant, but I know that Mary isn't.",
  "acknowledge":"acknowledged that Mary is pregnant, but I know that Mary isn't.",
  "admit":"admitted that Mary is pregnant, but I know that Mary isn't.",
  "confirm":"confirmed that Mary is pregnant, but I know that Mary isn't.",
  "be_right_that":"is right that that Mary is pregnant, but I know that Mary isn't."
  },
  "2": {
  "annoyed":"is annoyed that Josie went on vacation to France, but I know that Josie didn't.",
  "know":"knows that Josie went on vacation to France, but I know that Josie didn't.",
  "discover":"discovered that Josie went on vacation to France, but I know that Josie didn't.",
  "reveal":"revealed that Josie went on vacation to France, but I know that Josie didn't.",
  "see" :"saw that Josie went on vacation to France, but I know that Josie didn't.",
  "establish":"established that Josie went on vacation to France, but I know that Josie didn't.",
  "pretend":"pretended that Josie went on vacation to France, but I know that Josie didn't.",
  "think":"thinks that Josie went on vacation to France, but I know that Josie didn't.",
  "suggest":"suggested that Josie went on vacation to France, but I know that Josie didn't.",
  "prove":"proved that Josie went on vacation to France, but I know that Josie didn't.",
  "demonstrate":"demonstrated that Josie went on vacation to France, but I know that Josie didn't.",
  "say":"said that Josie went on vacation to France, but I know that Josie didn't.",
  "hear":"heard that Josie went on vacation to France, but I know that Josie didn't.",
  "confess":"confessed that Josie went on vacation to France, but I know that Josie didn't.",
  "inform_Sam":"informed Sam that Josie went on vacation to France, but I know that Josie didn't.",
  "announce":"announced that Josie went on vacation to France, but I know that Josie didn't.",
  "acknowledge":"acknowledged that Josie went on vacation to France, but I know that Josie didn't.",
  "admit":"admitted that Josie went on vacation to France, but I know that Josie didn't.",
  "confirm":"confirmed that Josie went on vacation to France, but I know that Josie didn't.",
  "be_right_that":"is right that that Josie went on vacation to France, but I know that Josie didn't."
  },
  "3": {
  "annoyed":"is annoyed that Emma studied on Saturday morning, but I know that Emma didn't.",
  "know":"knows that Emma studied on Saturday morning, but I know that Emma didn't.",
  "discover":"discovered that Emma studied on Saturday morning, but I know that Emma didn't.",
  "reveal":"revealed that Emma studied on Saturday morning, but I know that Emma didn't.",
  "see" :"saw that Emma studied on Saturday morning, but I know that Emma didn't.",
  "establish":"established that Emma studied on Saturday morning, but I know that Emma didn't.",
  "pretend":"pretended that Emma studied on Saturday morning, but I know that Emma didn't.",
  "think":"thinks that Emma studied on Saturday morning, but I know that Emma didn't.",
  "suggest":"suggested that Emma studied on Saturday morning, but I know that Emma didn't.",
  "prove":"proved that Emma studied on Saturday morning, but I know that Emma didn't.",
  "demonstrate":"demonstrated that Emma studied on Saturday morning, but I know that Emma didn't.",
  "say":"said that Emma studied on Saturday morning, but I know that Emma didn't.",
  "hear":"heard that Emma studied on Saturday morning, but I know that Emma didn't.",
  "confess":"confessed that Emma studied on Saturday morning, but I know that Emma didn't.",
  "inform_Sam":"informed Sam that Emma studied on Saturday morning, but I know that Emma didn't.",
  "announce":"announced that Emma studied on Saturday morning, but I know that Emma didn't.",
  "acknowledge":"acknowledged that Emma studied on Saturday morning, but I know that Emma didn't.",
  "admit":"admitted that Emma studied on Saturday morning, but I know that Emma didn't.",
  "confirm":"confirmed that Emma studied on Saturday morning, but I know that Emma didn't.",
  "be_right_that":"is right that that Emma studied on Saturday morning, but I know that Emma didn't."
  },
  "4": {
  "annoyed":"is annoyed that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "know":"knows that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "discover":"discovered that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "reveal":"revealed that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "see" :"saw that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "establish":"established that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "pretend":"pretended that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "think":"thinks that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "suggest":"suggested that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "prove":"proved that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "demonstrate":"demonstrated that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "say":"said that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "hear":"heard that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "confess":"confessed that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "inform_Sam":"informed Sam that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "announce":"announced that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "acknowledge":"acknowledged that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "admit":"admitted that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "confirm":"confirmed that Olivia sleeps until noon, but I know that Olivia doesn't.",
  "be_right_that":"is right that that Olivia sleeps until noon, but I know that Olivia doesn't."
  },
  "5": {
  "annoyed":"is annoyed that Sophia got a tattoo, but I know that Sophia didn't.",
  "know":"knows that Sophia got a tattoo, but I know that Sophia didn't.",
  "discover":"discovered that Sophia got a tattoo, but I know that Sophia didn't.",
  "reveal":"revealed that Sophia got a tattoo, but I know that Sophia didn't.",
  "see" :"saw that Sophia got a tattoo, but I know that Sophia didn't.",
  "establish":"established that Sophia got a tattoo, but I know that Sophia didn't.",
  "pretend":"pretended that Sophia got a tattoo, but I know that Sophia didn't.",
  "think":"thinks that Sophia got a tattoo, but I know that Sophia didn't.",
  "suggest":"suggested that Sophia got a tattoo, but I know that Sophia didn't.",
  "prove":"proved that Sophia got a tattoo, but I know that Sophia didn't.",
  "demonstrate":"demonstrated that Sophia got a tattoo, but I know that Sophia didn't.",
  "say":"said that Sophia got a tattoo, but I know that Sophia didn't.",
  "hear":"heard that Sophia got a tattoo, but I know that Sophia didn't.",
  "confess":"confessed that Sophia got a tattoo, but I know that Sophia didn't.",
  "inform_Sam":"informed Sam that Sophia got a tattoo, but I know that Sophia didn't.",
  "announce":"announced that Sophia got a tattoo, but I know that Sophia didn't.",
  "acknowledge":"acknowledged that Sophia got a tattoo, but I know that Sophia didn't.",
  "admit":"admitted that Sophia got a tattoo, but I know that Sophia didn't.",
  "confirm":"confirmed that Sophia got a tattoo, but I know that Sophia didn't.",
  "be_right_that":"is right that that Sophia got a tattoo, but I know that Sophia didn't."
  },
  "6": {
  "annoyed":"is annoyed that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "know":"knows that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "discover":"discovered that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "reveal":"revealed that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "see" :"saw that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "establish":"established that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "pretend":"pretended that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "think":"thinks that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "suggest":"suggested that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "prove":"proved that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "demonstrate":"demonstrated that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "say":"said that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "hear":"heard that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "confess":"confessed that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "inform_Sam":"informed Sam that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "announce":"announced that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "acknowledge":"acknowledged that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "admit":"admitted that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "confirm":"confirmed that Mia drank 2 cocktails last night, but I know that Mia  didn't.",
  "be_right_that":"is right that that Mia drank 2 cocktails last night, but I know that Mia  didn't."
  },
  "7": {
  "annoyed":"is annoyed that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "know":"knows that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "discover":"discovered that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "reveal":"revealed that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "see" :"saw that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "establish":"established that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "pretend":"pretended that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "think":"thinks that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "suggest":"suggested that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "prove":"proved that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "demonstrate":"demonstrated that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "say":"said that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "hear":"heard that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "confess":"confessed that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "inform_Sam":"informed Sam that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "announce":"announced that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "acknowledge":"acknowledged that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "admit":"admitted that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "confirm":"confirmed that Isabella ate a steak on Sunday, but I know that Isabella  didn't.",
  "be_right_that":"is right that Isabella ate a steak on Sunday, but I know that Isabella  didn't."
  },
  "8": {
  "annoyed":"is annoyed that Emily bought a car yesterday, but I know that Emily  didn't.",
  "know":"knows that Emily bought a car yesterday, but I know that Emily  didn't.",
  "discover":"discovered that Emily bought a car yesterday, but I know that Emily  didn't.",
  "reveal":"revealed that Emily bought a car yesterday, but I know that Emily  didn't.",
  "see" :"saw that Emily bought a car yesterday, but I know that Emily  didn't.",
  "establish":"established that Emily bought a car yesterday, but I know that Emily  didn't.",
  "pretend":"pretended that Emily bought a car yesterday, but I know that Emily  didn't.",
  "think":"thinks that Emily bought a car yesterday, but I know that Emily  didn't.",
  "suggest":"suggested that Emily bought a car yesterday, but I know that Emily  didn't.",
  "prove":"proved that Emily bought a car yesterday, but I know that Emily  didn't.",
  "demonstrate":"demonstrated that Emily bought a car yesterday, but I know that Emily  didn't.",
  "say":"said that Emily bought a car yesterday, but I know that Emily  didn't.",
  "hear":"heard that Emily bought a car yesterday, but I know that Emily  didn't.",
  "confess":"confessed that Emily bought a car yesterday, but I know that Emily  didn't.",
  "inform_Sam":"informed Sam that Emily bought a car yesterday, but I know that Emily  didn't.",
  "announce":"announced that Emily bought a car yesterday, but I know that Emily  didn't.",
  "acknowledge":"acknowledged that Emily bought a car yesterday, but I know that Emily  didn't.",
  "admit":"admitted that Emily bought a car yesterday, but I know that Emily  didn't.",
  "confirm":"confirmed that Emily bought a car yesterday, but I know that Emily  didn't.",
  "be_right_that":"is right that Emily bought a car yesterday, but I know that Emily  didn't."
  },
  "9": {
  "annoyed":"is annoyed that Grace visited her sister, but I know that Grace  didn't.",
  "know":"knows that Grace visited her sister, but I know that Grace  didn't.",
  "discover":"discovered that Grace visited her sister, but I know that Grace  didn't.",
  "reveal":"revealed that Grace visited her sister, but I know that Grace  didn't.",
  "see" :"saw that Grace visited her sister, but I know that Grace  didn't.",
  "establish":"established that Grace visited her sister, but I know that Grace  didn't.",
  "pretend":"pretended that Grace visited her sister, but I know that Grace  didn't.",
  "think":"thinks that Grace visited her sister, but I know that Grace  didn't.",
  "suggest":"suggested that Grace visited her sister, but I know that Grace  didn't.",
  "prove":"proved that Grace visited her sister, but I know that Grace  didn't.",
  "demonstrate":"demonstrated that Grace visited her sister, but I know that Grace  didn't.",
  "say":"said that Grace visited her sister, but I know that Grace  didn't.",
  "hear":"heard that Grace visited her sister, but I know that Grace  didn't.",
  "confess":"confessed that Grace visited her sister, but I know that Grace  didn't.",
  "inform_Sam":"informed Sam that Grace visited her sister, but I know that Grace  didn't.",
  "announce":"announced that Grace visited her sister, but I know that Grace  didn't.",
  "acknowledge":"acknowledged that Grace visited her sister, but I know that Grace  didn't.",
  "admit":"admitted that Grace visited her sister, but I know that Grace  didn't.",
  "confirm":"confirmed that Grace visited her sister, but I know that Grace  didn't.",
  "be_right_that":"is right that Grace visited her sister, but I know that Grace  didn't."
  },
  "10": {
  "annoyed":"is annoyed that Zoe calculated the tip, but I know that Zoe  didn't.",
  "know":"knows that Zoe calculated the tip, but I know that Zoe  didn't.",
  "discover":"discovered that Zoe calculated the tip, but I know that Zoe  didn't.",
  "reveal":"revealed that Zoe calculated the tip, but I know that Zoe  didn't.",
  "see" :"saw that Zoe calculated the tip, but I know that Zoe  didn't.",
  "establish":"established that Zoe calculated the tip, but I know that Zoe  didn't.",
  "pretend":"pretended that Zoe calculated the tip, but I know that Zoe  didn't.",
  "think":"thinks that Zoe calculated the tip, but I know that Zoe  didn't.",
  "suggest":"suggested that Zoe calculated the tip, but I know that Zoe  didn't.",
  "prove":"proved that Zoe calculated the tip, but I know that Zoe  didn't.",
  "demonstrate":"demonstrated that Zoe calculated the tip, but I know that Zoe  didn't.",
  "say":"said that Zoe calculated the tip, but I know that Zoe  didn't.",
  "hear":"heard that Zoe calculated the tip, but I know that Zoe  didn't.",
  "confess":"confessed that Zoe calculated the tip, but I know that Zoe  didn't.",
  "inform_Sam":"informed Sam that Zoe calculated the tip, but I know that Zoe  didn't.",
  "announce":"announced that Zoe calculated the tip, but I know that Zoe  didn't.",
  "acknowledge":"acknowledged that Zoe calculated the tip, but I know that Zoe  didn't.",
  "admit":"admitted that Zoe calculated the tip, but I know that Zoe  didn't.",
  "confirm":"confirmed that Zoe calculated the tip, but I know that Zoe  didn't.",
  "be_right_that":"is right that Zoe calculated the tip, but I know that Zoe  didn't."
  },
  "11": {
  "annoyed":"is annoyed that Danny ate the last cupcake, but I know that Danny  didn't.",
  "know":"knows that Danny ate the last cupcake, but I know that Danny  didn't.",
  "discover":"discovered that Danny ate the last cupcake, but I know that Danny  didn't.",
  "reveal":"revealed that Danny ate the last cupcake, but I know that Danny  didn't.",
  "see" :"saw that Danny ate the last cupcake, but I know that Danny  didn't.",
  "establish":"established that Danny ate the last cupcake, but I know that Danny  didn't.",
  "pretend":"pretended that Danny ate the last cupcake, but I know that Danny  didn't.",
  "think":"thinks that Danny ate the last cupcake, but I know that Danny  didn't.",
  "suggest":"suggested that Danny ate the last cupcake, but I know that Danny  didn't.",
  "prove":"proved that Danny ate the last cupcake, but I know that Danny  didn't.",
  "demonstrate":"demonstrated that Danny ate the last cupcake, but I know that Danny  didn't.",
  "say":"said that Danny ate the last cupcake, but I know that Danny  didn't.",
  "hear":"heard that Danny ate the last cupcake, but I know that Danny  didn't.",
  "confess":"confessed that Danny ate the last cupcake, but I know that Danny  didn't.",
  "inform_Sam":"informed Sam that Danny ate the last cupcake, but I know that Danny  didn't.",
  "announce":"announced that Danny ate the last cupcake, but I know that Danny  didn't.",
  "acknowledge":"acknowledged that Danny ate the last cupcake, but I know that Danny  didn't.",
  "admit":"admitted that Danny ate the last cupcake, but I know that Danny  didn't.",
  "confirm":"confirmed that Danny ate the last cupcake, but I know that Danny  didn't.",
  "be_right_that":"is right that that Danny ate the last cupcake, but I know that Danny  didn't."
  },
  "12": {
  "annoyed":"is annoyed that Frank got a cat, but I know that Frank   didn't.",
  "know":"knows that Frank got a cat, but I know that Frank   didn't.",
  "discover":"discovered that Frank got a cat, but I know that Frank   didn't.",
  "reveal":"revealed that Frank got a cat, but I know that Frank   didn't.",
  "see" :"saw that Frank got a cat, but I know that Frank   didn't.",
  "establish":"established that Frank got a cat, but I know that Frank   didn't.",
  "pretend":"pretended that Frank got a cat, but I know that Frank   didn't.",
  "think":"thinks that Frank got a cat, but I know that Frank   didn't.",
  "suggest":"suggested that Frank got a cat, but I know that Frank   didn't.",
  "prove":"proved that Frank got a cat, but I know that Frank   didn't.",
  "demonstrate":"demonstrated that Frank got a cat, but I know that Frank   didn't.",
  "say":"said that Frank got a cat, but I know that Frank   didn't.",
  "hear":"heard that Frank got a cat, but I know that Frank   didn't.",
  "confess":"confessed that Frank got a cat, but I know that Frank   didn't.",
  "inform_Sam":"informed Sam that Frank got a cat, but I know that Frank   didn't.",
  "announce":"announced that Frank got a cat, but I know that Frank   didn't.",
  "acknowledge":"acknowledged that Frank got a cat, but I know that Frank   didn't.",
  "admit":"admitted that Frank got a cat, but I know that Frank   didn't.",
  "confirm":"confirmed that Frank got a cat, but I know that Frank   didn't.",
  "be_right_that":"is right that Frank got a cat, but I know that Frank   didn't."
  },
  "13": {
  "annoyed":"is annoyed that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "know":"knows that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "discover":"discovered that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "reveal":"revealed that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "see" :"saw that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "establish":"established that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "pretend":"pretended that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "think":"thinks that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "suggest":"suggested that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "prove":"proved that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "demonstrate":"demonstrated that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "say":"said that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "hear":"heard that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "confess":"confessed that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "inform_Sam":"informed Sam that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "announce":"announced that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "acknowledge":"acknowledged that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "admit":"admitted that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "confirm":"confirmed that Jackson ran 10 miles, but I know that Jackson   didn't.",
  "be_right_that":"is right that that Jackson ran 10 miles, but I know that Jackson   didn't."
  },
  "14": {
  "annoyed":"is annoyed that Jayden rented a car, but I know that Jayden   didn't.",
  "know":"knows that Jayden rented a car, but I know that Jayden   didn't.",
  "discover":"discovered that Jayden rented a car, but I know that Jayden   didn't.",
  "reveal":"revealed that Jayden rented a car, but I know that Jayden   didn't.",
  "see" :"saw that Jayden rented a car, but I know that Jayden   didn't.",
  "establish":"established that Jayden rented a car, but I know that Jayden   didn't.",
  "pretend":"pretended that Jayden rented a car, but I know that Jayden   didn't.",
  "think":"thinks that Jayden rented a car, but I know that Jayden   didn't.",
  "suggest":"suggested that Jayden rented a car, but I know that Jayden   didn't.",
  "prove":"proved that Jayden rented a car, but I know that Jayden   didn't.",
  "demonstrate":"demonstrated that Jayden rented a car, but I know that Jayden   didn't.",
  "say":"said that Jayden rented a car, but I know that Jayden   didn't.",
  "hear":"heard that Jayden rented a car, but I know that Jayden   didn't.",
  "confess":"confessed that Jayden rented a car, but I know that Jayden   didn't.",
  "inform_Sam":"informed Sam that Jayden rented a car, but I know that Jayden   didn't.",
  "announce":"announced that Jayden rented a car, but I know that Jayden   didn't.",
  "acknowledge":"acknowledged that Jayden rented a car, but I know that Jayden   didn't.",
  "admit":"admitted that Jayden rented a car, but I know that Jayden   didn't.",
  "confirm":"confirmed that Jayden rented a car, but I know that Jayden   didn't.",
  "be_right_that":"is right that Jayden rented a car, but I know that Jayden   didn't."
  },
  "15": {
  "annoyed":"is annoyed that Tony had a drink last night, but I know that Tony   didn't.",
  "know":"knows that Tony had a drink last night, but I know that Tony   didn't.",
  "discover":"discovered that Tony had a drink last night, but I know that Tony   didn't.",
  "reveal":"revealed that Tony had a drink last night, but I know that Tony   didn't.",
  "see" :"saw that Tony had a drink last night, but I know that Tony   didn't.",
  "establish":"established that Tony had a drink last night, but I know that Tony   didn't.",
  "pretend":"pretended that Tony had a drink last night, but I know that Tony   didn't.",
  "think":"thinks that Tony had a drink last night, but I know that Tony   didn't.",
  "suggest":"suggested that Tony had a drink last night, but I know that Tony   didn't.",
  "prove":"proved that Tony had a drink last night, but I know that Tony   didn't.",
  "demonstrate":"demonstrated that Tony had a drink last night, but I know that Tony   didn't.",
  "say":"said that Tony had a drink last night, but I know that Tony   didn't.",
  "hear":"heard that Tony had a drink last night, but I know that Tony   didn't.",
  "confess":"confessed that Tony had a drink last night, but I know that Tony   didn't.",
  "inform_Sam":"informed Sam that Tony had a drink last night, but I know that Tony   didn't.",
  "announce":"announced that Tony had a drink last night, but I know that Tony   didn't.",
  "acknowledge":"acknowledged that Tony had a drink last night, but I know that Tony   didn't.",
  "admit":"admitted that Tony had a drink last night, but I know that Tony   didn't.",
  "confirm":"confirmed that Tony had a drink last night, but I know that Tony   didn't.",
  "be_right_that":"is right that Tony had a drink last night, but I know that Tony   didn't."
  },
  "16": {
  "annoyed":"is annoyed that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "know":"knows that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "discover":"discovered that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "reveal":"revealed that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "see" :"saw that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "establish":"established that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "pretend":"pretended that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "think":"thinks that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "suggest":"suggested that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "prove":"proved that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "demonstrate":"demonstrated that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "say":"said that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "hear":"heard that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "confess":"confessed that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "inform_Sam":"informed Sam that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "announce":"announced that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "acknowledge":"acknowledged that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "admit":"admitted that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "confirm":"confirmed that Josh learned to ride a bike yesterday, but I know that Josh   didn't. ",
  "be_right_that":"is right that that Josh learned to ride a bike yesterday, but I know that Josh   didn't. "
  },
  "17": {
  "annoyed":"is annoyed that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "know":"knows that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "discover":"discovered that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "reveal":"revealed that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "see" :"saw that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "establish":"established that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "pretend":"pretended that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "think":"thinks that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "suggest":"suggested that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "prove":"proved that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "demonstrate":"demonstrated that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "say":"said that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "hear":"heard that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "confess":"confessed that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "inform_Sam":"informed Sam that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "announce":"announced that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "acknowledge":"acknowledged that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "admit":"admitted that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "confirm":"confirmed that Owen shoveled snow last winter, but I know that Owen   didn't. ",
  "be_right_that":"is right that Owen shoveled snow last winter, but I know that Owen   didn't. "
  },
  "18": {
  "annoyed":"is annoyed that Julian dances salsa, but I know that Julian   doesn't. ",
  "know":"knows that Julian dances salsa, but I know that Julian   doesn't. ",
  "discover":"discovered that Julian dances salsa, but I know that Julian   doesn't. ",
  "reveal":"revealed that Julian dances salsa, but I know that Julian   doesn't. ",
  "see" :"saw that Julian dances salsa, but I know that Julian   doesn't. ",
  "establish":"established that Julian dances salsa, but I know that Julian   doesn't. ",
  "pretend":"pretended that Julian dances salsa, but I know that Julian   doesn't. ",
  "think":"thinks that Julian dances salsa, but I know that Julian   doesn't. ",
  "suggest":"suggested that Julian dances salsa, but I know that Julian   doesn't. ",
  "prove":"proved that Julian dances salsa, but I know that Julian   doesn't. ",
  "demonstrate":"demonstrated that Julian dances salsa, but I know that Julian   doesn't. ",
  "say":"said that Julian dances salsa, but I know that Julian   doesn't. ",
  "hear":"heard that Julian dances salsa, but I know that Julian   doesn't. ",
  "confess":"confessed that Julian dances salsa, but I know that Julian   doesn't. ",
  "inform_Sam":"informed Sam that Julian dances salsa, but I know that Julian   doesn't. ",
  "announce":"announced that Julian dances salsa, but I know that Julian   doesn't. ",
  "acknowledge":"acknowledged that Julian dances salsa, but I know that Julian   doesn't. ",
  "admit":"admitted that Julian dances salsa, but I know that Julian   doesn't. ",
  "confirm":"confirmed that Julian dances salsa, but I know that Julian   doesn't. ",
  "be_right_that":"is right that Julian dances salsa, but I know that Julian   doesn't. "
  },
  "19": {
  "annoyed":"is annoyed that Jon walks to work, but I know that Jon   doesn't. ",
  "know":"knows that Jon walks to work, but I know that Jon   doesn't. ",
  "discover":"discovered that Jon walks to work, but I know that Jon   doesn't. ",
  "reveal":"revealed that Jon walks to work, but I know that Jon   doesn't. ",
  "see" :"saw that Jon walks to work, but I know that Jon   doesn't. ",
  "establish":"established that Jon walks to work, but I know that Jon   doesn't. ",
  "pretend":"pretended that Jon walks to work, but I know that Jon   doesn't. ",
  "think":"thinks that Jon walks to work, but I know that Jon   doesn't. ",
  "suggest":"suggested that Jon walks to work, but I know that Jon   doesn't. ",
  "prove":"proved that Jon walks to work, but I know that Jon   doesn't. ",
  "demonstrate":"demonstrated that Jon walks to work, but I know that Jon   doesn't. ",
  "say":"said that Jon walks to work, but I know that Jon   doesn't. ",
  "hear":"heard that Jon walks to work, but I know that Jon   doesn't. ",
  "confess":"confessed that Jon walks to work, but I know that Jon   doesn't. ",
  "inform_Sam":"informed Sam that Jon walks to work, but I know that Jon   doesn't. ",
  "announce":"announced that Jon walks to work, but I know that Jon   doesn't. ",
  "acknowledge":"acknowledged that Jon walks to work, but I know that Jon   doesn't. ",
  "admit":"admitted that Jon walks to work, but I know that Jon   doesn't. ",
  "confirm":"confirmed that Jon walks to work, but I know that Jon   doesn't. ",
  "be_right_that":"is right that Jon walks to work, but I know that Jon   doesn't. "
  },
  "20": {
  "annoyed":"is annoyed that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "know":"knows that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "discover":"discovered that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "reveal":"revealed that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "see" :"saw that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "establish":"established that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "pretend":"pretended that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "think":"thinks that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "suggest":"suggested that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "prove":"proved that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "demonstrate":"demonstrated that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "say":"said that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "hear":"heard that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "confess":"confessed that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "inform_Sam":"informed Sam that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "announce":"announced that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "acknowledge":"acknowledged that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "admit":"admitte that Charley speaks Spanish, but I know that Charley   doesn't. d",
  "confirm":"confirmed that Charley speaks Spanish, but I know that Charley   doesn't. ",
  "be_right_that":"is right that Charley speaks Spanish, but I know that Charley   doesn't. "
  }
};
  
var items_content_mapping = {
"annoyed":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"know":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"discover":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"reveal":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"see":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"establish":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"pretend":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],  
"think":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"suggest":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"prove":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"demonstrate":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"say":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "hear":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"confess":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "inform_Sam":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"announce":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "acknowledge":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"admit":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "confirm":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"be_right_that":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"]
};  
 

// get trigger contents
  function getContent(trigger) {
//  		console.log("items_content_mapping before throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}  		
//  		console.log("items_content_mapping at the trigger before shuffling");
//  		console.log(items_content_mapping[trigger]);  		
  		items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
//  		console.log("items_content_mapping at the trigger after shuffling");
//  		console.log(items_content_mapping[trigger]);  		  		
//  		console.log("items_content_mapping after shuffling "+trigger);
//  		console.log(items_content_mapping);
  		var content = items_content_mapping[trigger].shift();//items_content_mapping[trigger][0];
//  		console.log("this is the selected content: " + content);
//		var index = items_content_mapping[trigger].indexOf(content);  		
//  		items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//  		console.log("items_content_mapping at the trigger after throwing it out");
//  		console.log(items_content_mapping[trigger]);  		  		
  		for (var j in items_content_mapping) {
			var index = items_content_mapping[j].indexOf(content);  
//			console.log("the next three lines: the array before removal, the index of content, the array after removal")
//			console.log(items_content_mapping[j]);
//			console.log(index);		
			if (index != -1)
			{			  			
				items_content_mapping[j].splice(index,1);			
			}
//			console.log(items_content_mapping[j]);			
  		}
//  		console.log("items_content_mapping after throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}   		  		
  		return content;
  	}

// assign contents to triggers
  var trigger_contents = {
  	"annoyed": getContent("annoyed"),  	  	
  	"know": getContent("know"),
  	"discover": getContent("discover"),  	  	
  	"reveal": getContent("reveal"),
  	"see": getContent("see"),
  	"establish": getContent("establish"),
  	"pretend": getContent("pretend"),  	
  	"think": getContent("think"),  	
  	"suggest": getContent("suggest"),
  	"prove": getContent("prove"),
  	"demonstrate": getContent("demonstrate"),
  	"say": getContent("say"),
  	"hear": getContent("hear"),
  	"confess": getContent("confess"),  	
  	"inform_Sam": getContent("inform_Sam"),
  	"announce": getContent("announce"),
  	"acknowledge": getContent("acknowledge"),
  	"admit": getContent("admit"),
  	"confirm": getContent("confirm"),
  	"be_right_that": getContent("be_right_that")
  	};
  
  function makeStim(i) {
    //get item
    var item = items[i];
	//get a speaker
    var name_data = speaker_names[i];
    var name = name_data.name;
    var gender = name_data.gender;
    //get another speaker
    var name_data2 = subject_names[i];
    var name2 = name_data2.name;
    var gender2 = name_data2.gender;
    
    // get content
    var trigger_cont = trigger_contents[item.trigger];
    var trigger = item.trigger;
    var short_trigger = trigger;
    if (trigger.indexOf("MC") != -1) {
    	short_trigger = "MC";
    	}
//	console.log("short_trigger: "+short_trigger);
//	console.log("trigger: "+trigger);
//    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = contents[trigger_cont][short_trigger];
    var question = contents[trigger_cont].question;   
//    console.log(contents[trigger_cont]); 
    return {
	  "name": name,
	  "name2": name2,
	  "gender": gender,	
	  "gender2": gender2,  
	  "trigger": item.trigger,
	  "short_trigger": short_trigger,	  
	  "trigger_class": item.trigger_class,
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }
  exp.stims_block1 = [];
//   exp.stims_block2 = []; 
  for (var i=0; i<items.length; i++) {
  	var stim = makeStim(i);
//    exp.stims_block1.push(makeStim(i));
	exp.stims_block1.push(jQuery.extend(true, {}, stim));
//	exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }  
  
console.log(exp.stims_block1);
//console.log(exp.stims_block2);   

	exp.stims_block1 = _.shuffle(exp.stims_block1);  
//	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
// decide which block comes first
//   var block_order = _.shuffle(["ai","projective"]);
//   var block1type = block_order[0];
//   var block2type = block_order[1];  
//   console.log(block_order);
//   console.log(block1type);  
//   console.log(block2type);    
// 
//    for (k in exp.stims_block2) {
//    		exp.stims_block2[k].block = block2type;//block_order[1];   	
//    	}
//    	
//    for (i in exp.stims_block1) {
//    		exp.stims_block1[i].block = block1type;//block_order[0];   	
//    	}


console.log(exp.stims_block1);
//console.log(exp.stims_block2);   	

//  exp.all_stims = [];
//  for (var i=0; i<items.length; i++) {
//    exp.all_stims.push(makeStim(i));
//  }
//
//	for (k in exp.all_stims) {
//		console.log(exp.all_stims[k].content)
//		}

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "block1", 'questionaire', 'finished'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 2 + 20 + 1; 
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}