- AI chat window
    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FR_8icV-Qup.png?alt=media&token=caeb1c0f-0662-4ddf-9909-d9195c161291)
    - Divided into 3 blocks:
        - Context (top grey background one)
            - Here you put the context if you want the chat to have some e.g a roam page, a block uid or a block embed. The plugin will automatically extract the notes from the page's or the children of a block uid.
        - Message (middle blue background one)
            - This is where the chat messages between the user and llm will be shown, user's message are prefixed with **User:** and the reply from llms with **Assistant**
            - There are 3 buttons (excluding `Tokens used: xxx`) at the bottom of the messages block
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FX8pFtQltCD.png?alt=media&token=0a30a897-0409-4736-b429-8da4e461af29)
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2Fl1V43Bf8Fu.png?alt=media&token=293e2dfa-10c1-431f-a283-8d01f418d2d1) Downward arrow: When pressed scrolls the chat window to the bottom i.e showing the latest chat messages.
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FCxWiSOkRgL.png?alt=media&token=b152443a-838f-44f1-bec9-fbfc3f2dacdf)Upwards arrow: When pressed scrolls the chat window to the top i.e showing the oldest chat messages.
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2Fv8hw4wkLU4.png?alt=media&token=55343cb6-4c3d-435a-86d0-81bbb343eac3)Link icon (third from left):
                    - Once you click this icon a new widget will be added to your existing chat messages which looks like this.
                        -  ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FEnQe2PCBrz.png?alt=media&token=254d09cc-d274-4e44-920e-056d202fb725)
                    - The Description for this widget is covered later under `Discourse graph this page` button. You can skip this for now.
                    -  Based on the chat conversation, the llm will propose a list of discourse nodes that can be made, we can then select the ones we want to convert from a llm suggestion to a new discourse node in our graph.
        - Chat (the pink one)
            - Here you can put down your query and chat with the llm.
            - We have 2 options here
                - Your custom message
                    - You write down your message, under the Chat block
                        - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2F8THbIaTLKf.png?alt=media&token=0dbeeb58-03f6-4bb5-8821-77160bab9f99)
                        - You don't have to write your message in one block, it can be multi block as well
                            - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FDEA5wIQ8kr.png?alt=media&token=cf8c4445-9ab6-49c2-9a91-8b2760ae0c90)
                    - Then you can customise what llm, response length, temperature to use for this chat using the gear icon.
                        - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FwOXMpAI-Ix.png?alt=media&token=c1baa62d-bb32-4183-a404-24b03fa063fd)
                        - Clicking on the gear icon open up the settings menu
                            - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FnFwMXlqVdT.png?alt=media&token=cfb575c2-dede-4c10-94a2-8895aae646f7)
                            - From here we have options to select
                                - The model we want to use for our chat
                                    - These are the models that we currently support, with plans to add more.
                                    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FBwpwk73XV6.png?alt=media&token=a17439a7-c0b2-4afc-9d95-86cbfcf7b5cf)
                                -  The output length of llm's response
                                - Temperature
                                - "Get linked refs?"
                                    - Check this button if for each page or block uid in Context (top grey background one), if you want to include references made to other discourse nodes from that page.
                                - "Extract query pages?"
                                    - When checked, if you put any page in the context, and that page has a query block in it then we will extractall the results of the query block. For each result, if the result is a page then we extract the whole content of that result's page and if the result is a block ref we extract the content of the block ref.
                                - "Extract query pages refs?"
                                    - This builds on top of the previous button `Extract query results?`. For each page result in the query builder's list of results, we also extract that result page's linked discourse node references.
                                    - so we include context 3 levels deep: current page -> each page in the query's result -> for each page in query's result its linked discourse node.
                    - Then to send your message to the llm you can either
                        - click on ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2F4OIqicqusX.png?alt=media&token=117bca41-c957-49e3-8737-6c14753e6b7a) to send your message to llm and wait for its reply.
                        - or while being under the chat block press "option + enter" to send the message to llm.
                - Pre build action buttons
                    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FVEmna6Cm0F.png?alt=media&token=511594c7-49f7-4c26-8675-049f9bad41e5)
                    - "Get context":
                        - Summarise whats is in the context window, by default also includes the linked discourse node references as context and if there is a query then we include all the context from the query result pages as well.
                            - We use Gemini-1.5-flash because its low cost and large context, since we are including context 3 levels deep.
                        - the goal is for users to be able to understand the basis for the eg Hypothesis or Issue, based on connected info, so that they can contribute, even if they didn't create it
                        - Once you click the button, you see a new user block added and then wait for the llm to respond.
                            - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FOmgxmznfRe.png?alt=media&token=f06274b5-b1b5-4701-a0a0-d4092cd352df)
                            - The prompt for this task is:
                                - ```javascript
                                  Please structure your response as follows:
                                  
                                  <summary>
                                  Provide a concise, high-level overview of the research topic, including:
                                  1. The main research question or focus
                                  2. The underlying motivation for this research
                                  3. The primary hypothesis or hypotheses
                                  4. Potential significance and implications of the research
                                  
                                  Base this summary primarily on the provided discourse node page content's body, supplemented by the linked references content. Use your expertise to offer insights and connections.
                                  </summary>
                                  
                                  <context>
                                  Elaborate on the research context:
                                  1. Explain how this topic fits within the larger research project or question
                                  2. Identify key concepts, methods, or findings relevant to this research
                                  3. Highlight any challenges or open questions in this area of study
                                  
                                  Explicitly cite relevant discourse nodes present in <discourse-node-page-content's body and its linked-refs when possible. You may also draw from your knowledge base to provide additional context or speculate on the significance.
                                  </context>
                                  <important-instruction> While referencing a node use roam format of double brackets e.g `[[page title]]` <example> [[[[[[ISS]] - this is issue]]]] </example>
                                  </important-instruction>
                                  Ensure your response is:
                                  1. Comprehensive and well-structured
                                  2. Accurately reflects the provided information
                                  3. Demonstrates deep understanding of endocytosis and related cell biology concepts
                                  4. Provides insightful analysis and connections between different aspects of the research
                                  5. Uses clear, scientific language appropriate for an academic audience
                                  ```
                    - "Get Suggestions":
                        - Here the LLM acts as a creative partner, providing ideas for next steps. It tries to follow our   discourse graph workflow: ie if the context includes an experiment,    try to identify observations (results) within the experiment page. if the context includes a result, try to    tie it to a claim. if you have a claim, propose next possible experiments (issues)
                            -  higher temperature, more creative, more sophisticated LLM chosen by default
                        - Once you click the button, you see a new user block added and then wait for the llm to respond.
                            - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FmlwkTAa4iT.png?alt=media&token=06c221bd-3d86-4da8-9673-bdba512cd4fa)
                        - The prompt for this task is:
                            - ```javascript
                              Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.
                              
                              We capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight. We call these discourse nodes.
                              
                              For example, 
                              
                              <example> 
                              a QUE page may ask "How does the Arp2/3 complex bind to actin filaments?" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to RES pages that either support or oppose the hypothesis.
                              </example>
                              
                              <types-of-discourse-nodes>
                              Question (QUE): A research inquiry or topic that needs investigation or synthesis.
                              Claim (CLM): A statement or assertion that is supported by evidence.
                              Evidence (EVD): Information, data, or observations that support or relate to a claim or question.
                              Hypothesis (HYP): A proposed explanation or prediction that can be tested through experiments or further research.
                              Result (RES): The outcome or finding from an experiment or study.
                              Experiment (EXP): A scientific procedure undertaken to test a hypothesis or answer a question.
                              Conclusion (CON): An interpretation or judgment drawn from results or evidence.
                              Issue (ISS): A problem, challenge, or topic that requires attention or investigation.
                              </types-of-discourse-nodes>
                              
                              
                              <discourse-graph-workflow>
                              The discourse graph workflow is an iterative process that structures scientific inquiry and knowledge building. It begins with an Issue, which leads to the formulation of a Hypothesis. This Hypothesis is then tested through an Experiment, which produces Results. These Results are interpreted to form Claims, which can contribute to broader Models or Theories. Each step naturally progresses to the next, creating a cycle of investigation and discovery. However, the workflow is flexible, allowing movement back to previous steps as new information emerges. This structure captures the complex, non-linear nature of scientific research while providing a clear path for advancement. The workflow encourages researchers to continually push their investigations forward, moving from questions to theories and back again, fostering a systematic approach to scientific understanding.
                              </discourse-graph-workflow>
                              
                              <your-job>
                              Based on the text and images provided, first determine where the context seems to fit in <discourse-graph-workflow>, then if it is:
                              
                              an Issue:
                              -> Formulate a Hypothesis
                              a Hypothesis:
                              -> Design an Experiment to test it
                              an Experiment:
                              -> Identify or prompt for Results (observations)
                              Ia Result:
                              -> Tie it to a Claim, interpreting its significance
                              a Claim:
                              -> Propose new Issues (possible experiments) to further investigate or validate the claim
                              -> Consider how it might contribute to or modify existing Models/Theories
                              a Model/Theory:
                              -> Generate new Issues or Hypotheses to test or refine the model
                              
                              The goal is to guide the user to next steps based on the current context. The user expects answers to question like "point me in next direction" "suggest next steps". They are looking for a detailed creative response that is firmly grounded in the provided context.
                              
                              </your-job>
                              
                              <general-important-instructions>
                              following the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. Make sure your {content} draws directly from the text and images provided.
                              Please use the linked-refs data also for your context. Take everything in context and provide a creative based answer. 
                              </general-important-instructions>
                              
                              
                              ```
    - Customisation
        - We can do individual chat customisation by using the gear icon as described in the Chat (the pink one) section.
        - {{[[TODO]]}} To change the default settings for any new chat window we can use the gear icon in the bottom bar.
        -
    - Since the chat uses roam blocks we can reference them in another page, embed etc. basically whatever we can do with a normal roam block.
- Bottom bar
    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FU8BEbnr38S.png?alt=media&token=223e858b-a3a9-4b0d-9665-6271a02de2e0)
    - Is a one click way to do llm actions in your graph. The bottom bar does not take space in your graph and shows at one glance what are the different actions you can do. You don't have to remember shortcuts or navigate a context menu, remember the feature list and job for each one of them. The buttons know which page or context you are in, therefore there are some buttons that only appear on certain type of pages e.g "summarise last 1:1 meeting" only appears on pages which are of type "Meetings lab-user-name and Matt"
    - The buttons are pre-configured with a specific job, the model, temperature, prompt, response length are already set you just have to press the button to do its job. You can customise the buttons using the gear icon so that each subsequent use of the button uses the new settings.
    - Currently there are following actions/commands you can do from it:
        - `Discourse graph this page`
            - This command would automate (more like assist or guide) in the process of generating a discourse graph from your current unstructured work, and encourage you to share it with others.
            -  Based on the current page's context or chat conversation (in case of using the ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2Fv8hw4wkLU4.png?alt=media&token=55343cb6-4c3d-435a-86d0-81bbb343eac3)Link icon (third from left): in AI chat window), the llm will suggest a list of discourse nodes that can be made, we can then select the ones we want to convert from a llm suggestion to a new discourse node in our graph.
            - Once you click this button a new block with text `AI Discourse node suggestions` will be added at end of your current page. Under the block you will see a widget
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FjPZIbLd91N.png?alt=media&token=f738f4f7-ec7a-4bc9-bba9-5c802b1b24cb)
                - Description
                    - These are the suggestions made my the llm based on the context it got (current page or chat conversation)
                    - For each suggestion we can either take a direct action (accept and discard) or drill down further to get more clarity and decision.
                        - Direct actions
                            - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FYIQ7XDzcej.png?alt=media&token=eac7c095-7442-4148-b8fa-a95a7be94470)
                            - Check: To accept the suggestion and convert it into a new discourse node. Once you check it and the node gets created then the suggestion will turn yellow implying that this suggestion is accepted.
                            - Cross: By using this you remove the suggestion from the list.
                        - Drill down
                            - The principle we want to follow is that its better to make more connections in the existing nodes that creating new nodes.
                            - So one tool we have at our disposal is vector db for semantic search. We put all of our existing discourse node's title in a vector db.
                            - Then for each suggestion we can "get top 3 semantically similar nodes". Once you have the llm discourse node suggestion with a list of semantically similar discourse nodes from the graph, you can make a decision if to turn the suggestion into a new node or not.
                                - How?
                                    - First we select the suggestions for which we want to find semantically similar dg nodes
                                        - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FGJuHj-ilNK.png?alt=media&token=53c34254-1516-4f44-abb1-b4288dd1e83b)
                                    - Then click on the button "Semantic search for selected suggestion"
                                        - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FGoMhThvNx2.png?alt=media&token=64b04b77-d9ac-453c-b22a-b3847bb30c82)
                                    - Then for each suggestion we will see its semantically similar nodes.
                                        - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FpUgnRogsNh.png?alt=media&token=6bff4cc9-7538-4754-bf51-a2e825a099a5)
                            - If you want to further drill down and see how the current network of nodes look like (visualise a subgraph), where does the suggestion and its similar nodes fit in the bigger discourse graph.
                                - We can select such suggestions using the checkbox
                                    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FGJuHj-ilNK.png?alt=media&token=53c34254-1516-4f44-abb1-b4288dd1e83b)
                                - Then click on the middle button, this button's text depends on if this is the first time you are visualising or you have already done it in the past. So you will either see "Visualise suggestion" or "Connect to existing visualisation".
                                - Once you click on the button a new widget will be shown below the current one.
                                    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2Fzzpd3jojdn.png?alt=media&token=2c79e95f-3774-4663-90a4-be17608cdaf5)
                                - The nodes (rectangles) with dotted outline and white background are the suggestions made by the llm, each arrow from the suggestion connects to one of the semantic similar nodes.
                                - Each semantic similar node is has different background color based on the type of the node, for e.g here HYP has green, QUE is blue, EVD is red.
                                - Each semantic similar node further has arrows from it, representing that nodes's existing discourse connections for e.g
                                    - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FK4APndfx7S.png?alt=media&token=60cccc62-a560-41d0-8905-a79e8842ba21)
                                - So now for each suggestion, we have a the visualisation of:
                                    - Suggestion --- connecting to ---> semantically similar node --- ---further connecting to its discourse connection ---> discourse node.
                                - Now we can see visually where the suggestion sits in the existing subgraph, and do operations (not implemented but planned) like
                                    - merge 2 nodes by dragging one on top other
                                    - make more dg connections within the subgraph
                                    - chat by using the nodes in visualisation as context
                                    - Using the visualisation as context ask llm to create new connections or suggest new subgraphs including not only nodes but also potential discourse graph edge (we have partially implemented this feature)
        - `Get context`
            - Same as "Get context":  in AI chat window but for the current page a user is in.
            - Once you click this button, a new block with text `AI: Get context` will be added at the end of your current page. The response from the llm will be added as a child block to `AI: Get context` block.
        - `Semantic search bar`
            - A way to do semantic search of all the discourse nodes in our graph.
            - Once you type in your search query, then you press the right-arrow button. The plugin will query the vector db and present you with the top 3 results, you can then select any one of the results to go that page.
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FYf-zNzI19Q.png?alt=media&token=c2445ef6-282e-40f3-843c-eb560a4f1bbe)
        - `Get Suggestions`
            - Same as "Get Suggestions": in AI chat window but for the current page a user is in.
            - Once you click this button, a new block with text `AI: Get suggestions for next step` will be added at the end of your current page. The response from the llm will be added as a child block to `AI: Get suggestions for next step` block.
        - `Chat with this page`
            - Adds the AI chat window to last block of the current page and opens it in the right sidebar. The context from the current page is automatically added under the `Context` area of chat window.
            - A default system prompt for each chat window in Matt's lab is:
                - ```javascript
                  This is Dr. Akamatsu's biology lab at the University of Washington. Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.
                  
                  We capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight, a body elaborating on findings and literature, and hierarchical references (refs) linking to related pages. The refs show the lineage of ideas from one page to detailed explorations on another.
                  
                  For example, a QUE page may ask "How does the Arp2/3 complex bind to actin filaments?" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to CON pages concluding whether our hypothesis was supported or refuted.
                  
                  Our pages integrate knowledge from publications, data visualizations, and discussions with experts in the field. By connecting the dots across pages, we maintain an audit trail of the evolution in our research.
                  
                  The provided page data reflects this structure, each individual page is a map with keys `:title`, `:body` and `:refs`. The body content combines biology expertise with our lab's own analyses and experimental data.
                  ```
        - `Settings (gear icon)`
            - We can use the settings to change the default parameters for `Discourse graph this page`, `Get context` and `Get Suggestions` commands.
                - ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2F9J8wYfA5dW.png?alt=media&token=6ec90202-7b98-4a7a-9c95-32062b467d3e)
                -
    - Previously we had
        - `Chat with selected pages`
            - Filter pages from Graph Overview and use them as context for chat with llm. You can use filters in graph overview to narrow down the pages that you want to use.
        - `Generate image descriptions`
            - Finds all the images in current page, then generate description for each image then adds this description as alt text to the corresponding images. This becomes useful when you want to use models that dont support vision-api, so we use the alt text to describe what's in the image this results in a context for llm with text and no image.
        - `Start new chat`
            - Adds the `AI chat window` to last block of the current page, no context included. Its like a general chat interface where you can have random chats not related to any particular page or you add the context as needed.
        - `Start new chat in focused block`
            - Adds the `AI chat window` in the block you are currently focused into, no context included.
        - `Start chat in daily notes, show in sidebar`
            - Adds the `AI chat window` in daily notes instead of current page and opens the window in right sidebar. You have a chat window open while you navigate different pages and use context from them to chat with.
        - `Summarise this page`
            - Summarises the current page, and puts the summary on the last block of the page. 
