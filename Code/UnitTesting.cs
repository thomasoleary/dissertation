using System.Collections;
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;

namespace Tests
{
    public class UnitTesting
    {
        private AgentManager agentManager;
        private GameObject[] agents;
        private int agentAmount;

        [SetUp]
        public void SetUp()
        {
            agentManager = GameObject.Find("Room").GetComponent<AgentManager>();
            agents = GameObject.FindGameObjectsWithTag("agent");

            agentAmount = agents.GetLength(0);
        }


        // Checks if the AgentManager exists in the scene
        [Test]
        public void AgentManagerExists()
        {
            Assert.IsNotNull(agentManager);
        }

        // Checks if all Agents are in the AgentManager
        [Test]
        public void AllAgentsInManager()
        {
            var tempManager = new GameObject().AddComponent<AgentManager>();

            if (tempManager.furnitureInScene.Count == agentManager.furnitureInScene.Count)
            {
                Assert.IsNotNull(tempManager);
            }
        }


        // Checks if there are Agents in the scene
        [Test]
        public void AgentsExists()
        {
            Assert.IsNotNull(agents);
        }

        // Checks to see if the Agents are initialised
        [Test]
        public void HasAgentInitialised()
        {
            foreach (var agent in agents)
            {
                if (agent.GetComponent<Agent>().agentSO.isInitialised)
                {
                    Assert.IsNotNull(agent.GetComponent<Agent>().agentSO.isInitialised);
                }
            }
        }

        // Checks to see if all Agents have placement transform set
        [Test]
        public void AgentHasPlacement()
        {
            foreach(var agent in agents)
            {
                if (agent.GetComponent<Agent>().agentSO.agentPlacement)
                {
                    Assert.IsNotNull(agent.GetComponent<Agent>().agentSO.agentPlacement);
                }
            }
        }


        // Checks to see if Agent is in SEARCH
        [Test]
        public void AgentStartInSearch()
        {
            foreach(var agent in agents)
            {
                if (agent.GetComponent<Agent>().agentSO.state == AgentState.SEARCH)
                {
                    Assert.IsNotNull(agent);
                }
            }
        }

        // Checks to see if Agent finds a parent
        [Test]
        public void AgentFindsParent()
        {
            foreach(var agent in agents)
            {
                if(agent.GetComponent<Agent>().agentSO.hasFoundParent)
                {
                    Assert.IsNotNull(agent);
                }
            }
        }

        // Checks to see if Agent is in REST
        [Test]
        public void AgentEndInRest()
        {
            foreach(var agent in agents)
            {
                if (agent.GetComponent<Agent>().agentSO.state == AgentState.REST)
                {
                    Assert.IsNotNull(agent);
                }
            }
        }
    }

}